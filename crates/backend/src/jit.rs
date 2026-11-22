use std::{
    collections::HashMap,
    mem,
    sync::{Arc, Mutex},
};

use cranelift::{
    codegen::{
        Context,
        ir::{
            Block, FuncRef, InstBuilder, MemFlagsData, StackSlot, StackSlotData, StackSlotKind,
            UserFuncName, Value as ClifValue, condcodes::IntCC, types,
        },
        settings::{self, Configurable},
    },
    prelude::{AbiParam, FunctionBuilder, FunctionBuilderContext, Variable},
};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{Linkage, Module as ClifModule, default_libcall_names};
use thiserror::Error;

use crate::{
    interpreter::Interpreter,
    ir::{Callable, CompareOp, Const, FuncId, HostId, Inst, Module, Reg, Type as IrType},
    runtime::HostFn,
    value::Value,
};

pub type CompiledFn = unsafe extern "C" fn(*mut JitContext, *const u64, *mut u64);
type ClifFuncId = cranelift_module::FuncId;

#[derive(Debug, Error)]
pub enum JitError {
    #[error("Cranelift error: {0}")]
    Cranelift(String),
    #[error("function {0} cannot be JIT-compiled yet")]
    Ineligible(FuncId),
    #[error("invalid jump target {target} in function {func}")]
    InvalidJump { func: FuncId, target: usize },
}

#[derive(Clone, Copy)]
#[repr(C)]
pub struct JitContext {
    pub module: *const Module,
    pub hosts: *const HostFn,
    pub host_count: usize,
    pub compiled: *const Mutex<HashMap<FuncId, *const u8>>,
}

pub struct JitState {
    module: JITModule,
    compiled: Arc<Mutex<HashMap<FuncId, *const u8>>>,
    hotness: Mutex<HashMap<FuncId, u32>>,
    threshold: u32,
    dispatch_call: ClifFuncId,
    dispatch_host: ClifFuncId,
    enabled: bool,
}

impl JitState {
    pub fn new(threshold: u32, enabled: bool) -> Result<Self, JitError> {
        let mut flag_builder = settings::builder();
        flag_builder
            .set("use_colocated_libcalls", "false")
            .map_err(|err| JitError::Cranelift(err.to_string()))?;
        let is_pic = if cfg!(target_arch = "x86_64") {
            "true"
        } else {
            "false"
        };
        flag_builder
            .set("is_pic", is_pic)
            .map_err(|err| JitError::Cranelift(err.to_string()))?;
        let isa = cranelift_native::builder()
            .map_err(|err| JitError::Cranelift(err.to_string()))?
            .finish(settings::Flags::new(flag_builder))
            .map_err(|err| JitError::Cranelift(err.to_string()))?;

        let mut jit_builder = JITBuilder::with_isa(isa, default_libcall_names());
        jit_builder.symbol("crust_dispatch_call", dispatch_call as *const u8);
        jit_builder.symbol("crust_dispatch_host", dispatch_host as *const u8);
        let mut module = JITModule::new(jit_builder);

        let mut dispatch_sig = module.make_signature();
        for _ in 0..4 {
            dispatch_sig.params.push(AbiParam::new(types::I64));
        }
        let dispatch_call = module
            .declare_function("crust_dispatch_call", Linkage::Import, &dispatch_sig)
            .map_err(|err| JitError::Cranelift(err.to_string()))?;
        let dispatch_host = module
            .declare_function("crust_dispatch_host", Linkage::Import, &dispatch_sig)
            .map_err(|err| JitError::Cranelift(err.to_string()))?;

        Ok(Self {
            module,
            compiled: Arc::new(Mutex::new(HashMap::new())),
            hotness: Mutex::new(HashMap::new()),
            threshold,
            dispatch_call,
            dispatch_host,
            enabled,
        })
    }

    pub fn context(&self, module: &Module, hosts: &[HostFn]) -> JitContext {
        JitContext {
            module: module as *const Module,
            hosts: hosts.as_ptr(),
            host_count: hosts.len(),
            compiled: Arc::as_ptr(&self.compiled),
        }
    }

    pub fn compiled_ptr(&self, func_id: FuncId) -> Option<*const u8> {
        self.compiled.lock().unwrap().get(&func_id).copied()
    }

    pub fn should_compile(&mut self, module: &Module, func_id: FuncId) -> bool {
        if !self.enabled || self.compiled_ptr(func_id).is_some() {
            return false;
        }
        let mut hotness = self.hotness.lock().unwrap();
        let count = hotness.entry(func_id).or_insert(0);
        *count += 1;
        *count >= self.threshold && is_eligible(module, func_id)
    }

    pub fn note_call(&mut self, module: &Module, func_id: FuncId) {
        if !self.enabled || self.compiled_ptr(func_id).is_some() {
            return;
        }
        let should_compile = {
            let mut hotness = self.hotness.lock().unwrap();
            let count = hotness.entry(func_id).or_insert(0);
            *count += 1;
            *count >= self.threshold && is_eligible(module, func_id)
        };
        if should_compile {
            let _ = self.compile_function(module, func_id);
        }
    }

    pub fn compile_function(&mut self, module: &Module, func_id: FuncId) -> Result<(), JitError> {
        let function = module
            .functions
            .get(func_id as usize)
            .ok_or(JitError::Ineligible(func_id))?;
        if !is_eligible(module, func_id) {
            return Err(JitError::Ineligible(func_id));
        }

        let mut sig = self.module.make_signature();
        for _ in 0..3 {
            sig.params.push(AbiParam::new(types::I64));
        }

        let clif_id = self
            .module
            .declare_function(&format!("crust_fn_{func_id}"), Linkage::Local, &sig)
            .map_err(|err| JitError::Cranelift(err.to_string()))?;

        let mut ctx = Context::new();
        ctx.func = cranelift::codegen::ir::Function::with_name_signature(
            UserFuncName::user(0, clif_id.as_u32()),
            sig,
        );
        let mut func_ctx = FunctionBuilderContext::new();
        {
            let mut builder = FunctionBuilder::new(&mut ctx.func, &mut func_ctx);
            let dispatch_call_ref = self
                .module
                .declare_func_in_func(self.dispatch_call, &mut builder.func);
            let dispatch_host_ref = self
                .module
                .declare_func_in_func(self.dispatch_host, &mut builder.func);
            let entry = builder.create_block();
            let blocks: Vec<Block> = (0..function.code.len())
                .map(|_| builder.create_block())
                .collect();
            if blocks.is_empty() {
                return Err(JitError::Cranelift(
                    "function has no instructions".to_string(),
                ));
            }

            let i64_ty = types::I64;
            // Each IR register becomes a Cranelift variable. FunctionBuilder
            // turns assignments that reach a block from multiple paths into
            // block parameters (phi nodes), so loops and conditionals live in
            // SSA instead of spilling every register to a stack slot.
            let reg_vars: Vec<Variable> = (0..function.nregs)
                .map(|index| Variable::from_u32(index as u32))
                .collect();
            let ctx_var = Variable::from_u32(function.nregs as u32);
            let args_var = Variable::from_u32(function.nregs as u32 + 1);
            let ret_var = Variable::from_u32(function.nregs as u32 + 2);
            for _ in 0..function.nregs + 3 {
                builder.declare_var(i64_ty);
            }

            builder.switch_to_block(entry);
            builder.append_block_params_for_function_params(entry);
            let params = builder.block_params(entry).to_vec();
            builder.def_var(ctx_var, params[0]);
            builder.def_var(args_var, params[1]);
            builder.def_var(ret_var, params[2]);
            let args_addr = builder.use_var(args_var);
            for (reg, var) in reg_vars.iter().enumerate().take(function.params.len()) {
                let value = builder
                    .ins()
                    .load(i64_ty, mem_flags(), args_addr, (reg * 8) as i32);
                builder.def_var(*var, value);
            }
            let zero = builder.ins().iconst(i64_ty, 0);
            for var in reg_vars.iter().skip(function.params.len()) {
                builder.def_var(*var, zero);
            }
            builder.ins().jump(blocks[0], &[]);

            for (index, inst) in function.code.iter().enumerate() {
                builder.switch_to_block(blocks[index]);
                let terminated = lower_inst(
                    &mut builder,
                    inst,
                    &reg_vars,
                    ctx_var,
                    ret_var,
                    &blocks,
                    blocks.get(index + 1).copied().unwrap_or(blocks[index]),
                    func_id,
                    dispatch_call_ref,
                    dispatch_host_ref,
                )?;
                if !terminated {
                    if let Some(next) = blocks.get(index + 1) {
                        builder.ins().jump(*next, &[]);
                    } else {
                        let zero = builder.ins().iconst(i64_ty, 0);
                        let ret_addr = builder.use_var(ret_var);
                        builder.ins().store(mem_flags(), zero, ret_addr, 0);
                        builder.ins().return_(&[]);
                    }
                }
            }

            builder.seal_all_blocks();
            builder.finalize(self.module.target_config());
        }

        self.module
            .define_function(clif_id, &mut ctx)
            .map_err(|err| JitError::Cranelift(err.to_string()))?;
        self.module.clear_context(&mut ctx);
        self.module
            .finalize_definitions()
            .map_err(|err| JitError::Cranelift(err.to_string()))?;
        let code = self.module.get_finalized_function(clif_id);
        self.compiled.lock().unwrap().insert(func_id, code);
        Ok(())
    }
}

pub struct JitBridge {
    state: Arc<Mutex<JitState>>,
    module: *const Module,
    hosts: *const HostFn,
    host_count: usize,
}

impl JitBridge {
    pub fn new(state: Arc<Mutex<JitState>>, module: &Module, hosts: &[HostFn]) -> Self {
        Self {
            state,
            module: module as *const Module,
            hosts: hosts.as_ptr(),
            host_count: hosts.len(),
        }
    }

    pub fn note_call(&self, func_id: FuncId) {
        let module = unsafe { &*self.module };
        let mut state = self.state.lock().unwrap();
        state.note_call(module, func_id);
    }

    pub fn call_if_compiled(&self, func_id: FuncId, args: &[Value]) -> Option<Value> {
        let code = {
            let state = self.state.lock().unwrap();
            state.compiled_ptr(func_id)?
        };
        let module = unsafe { &*self.module };
        let hosts = unsafe { std::slice::from_raw_parts(self.hosts, self.host_count) };
        let context = {
            let state = self.state.lock().unwrap();
            state.context(module, hosts)
        };
        let encoded = args
            .iter()
            .map(encode_value)
            .collect::<Result<Vec<_>, _>>()
            .ok()?;
        let mut result = 0u64;
        unsafe {
            let function: CompiledFn = mem::transmute(code);
            function(&mut context.clone(), encoded.as_ptr(), &mut result);
        }
        Some(decode_value(
            module.functions[func_id as usize].return_type,
            result,
        ))
    }
}

fn mem_flags() -> MemFlagsData {
    MemFlagsData::new()
}

fn bool_to_i64(builder: &mut FunctionBuilder<'_>, condition: ClifValue) -> ClifValue {
    let one = builder.ins().iconst(types::I64, 1);
    let zero = builder.ins().iconst(types::I64, 0);
    builder.ins().select(condition, one, zero)
}

fn make_slot(builder: &mut FunctionBuilder<'_>, size: u32) -> StackSlot {
    builder
        .func
        .create_sized_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, size, 3))
}

fn reg_value(
    builder: &mut FunctionBuilder<'_>,
    vars: &[Variable],
    reg: Reg,
) -> Result<ClifValue, JitError> {
    vars.get(reg as usize)
        .copied()
        .map(|var| builder.use_var(var))
        .ok_or(JitError::Cranelift(format!("invalid register r{reg}")))
}

fn set_reg(
    builder: &mut FunctionBuilder<'_>,
    vars: &[Variable],
    reg: Reg,
    value: ClifValue,
) -> Result<(), JitError> {
    match vars.get(reg as usize) {
        Some(var) => {
            builder.def_var(*var, value);
            Ok(())
        }
        None => Err(JitError::Cranelift(format!("invalid register r{reg}"))),
    }
}

fn const_bits(value: &Const) -> Result<i64, JitError> {
    match value {
        Const::Int(value) => Ok(*value),
        Const::Float(value) => Ok(value.to_bits() as i64),
        Const::Bool(value) => Ok(*value as i64),
        Const::Str(_) => Err(JitError::Cranelift(
            "strings are not supported in JIT code".to_string(),
        )),
    }
}

#[allow(clippy::too_many_arguments)]
fn lower_inst(
    builder: &mut FunctionBuilder<'_>,
    inst: &Inst,
    vars: &[Variable],
    ctx_var: Variable,
    ret_var: Variable,
    blocks: &[Block],
    next_block: Block,
    func_id: FuncId,
    dispatch_call: FuncRef,
    dispatch_host: FuncRef,
) -> Result<bool, JitError> {
    let i64_ty = types::I64;
    match inst {
        Inst::LoadConst { dst, value } => {
            let value = builder.ins().iconst(i64_ty, const_bits(value)?);
            set_reg(builder, vars, *dst, value)?;
        }
        Inst::Move { dst, src } => {
            let value = reg_value(builder, vars, *src)?;
            set_reg(builder, vars, *dst, value)?;
        }
        Inst::AddI { dst, lhs, rhs } => {
            let left = reg_value(builder, vars, *lhs)?;
            let right = reg_value(builder, vars, *rhs)?;
            let result = builder.ins().iadd(left, right);
            set_reg(builder, vars, *dst, result)?;
        }
        Inst::AddImm { dst, lhs, imm } => {
            let left = reg_value(builder, vars, *lhs)?;
            let result = builder.ins().iadd_imm_s(left, *imm);
            set_reg(builder, vars, *dst, result)?;
        }
        Inst::SubI { dst, lhs, rhs } => {
            let left = reg_value(builder, vars, *lhs)?;
            let right = reg_value(builder, vars, *rhs)?;
            let result = builder.ins().isub(left, right);
            set_reg(builder, vars, *dst, result)?;
        }
        Inst::SubImm { dst, lhs, imm } => {
            let left = reg_value(builder, vars, *lhs)?;
            let result = builder.ins().iadd_imm_s(left, -(*imm));
            set_reg(builder, vars, *dst, result)?;
        }
        Inst::MulI { dst, lhs, rhs } => {
            let left = reg_value(builder, vars, *lhs)?;
            let right = reg_value(builder, vars, *rhs)?;
            let result = builder.ins().imul(left, right);
            set_reg(builder, vars, *dst, result)?;
        }
        Inst::DivI { dst, lhs, rhs } => {
            let left = reg_value(builder, vars, *lhs)?;
            let right = reg_value(builder, vars, *rhs)?;
            let result = builder.ins().sdiv(left, right);
            set_reg(builder, vars, *dst, result)?;
        }
        Inst::ModI { dst, lhs, rhs } => {
            let left = reg_value(builder, vars, *lhs)?;
            let right = reg_value(builder, vars, *rhs)?;
            let result = builder.ins().srem(left, right);
            set_reg(builder, vars, *dst, result)?;
        }
        Inst::NegI { dst, src } => {
            let value = reg_value(builder, vars, *src)?;
            let result = builder.ins().ineg(value);
            set_reg(builder, vars, *dst, result)?;
        }
        Inst::AddF { dst, lhs, rhs } => {
            let lhs_bits = reg_value(builder, vars, *lhs)?;
            let left = float_value(builder, lhs_bits);
            let rhs_bits = reg_value(builder, vars, *rhs)?;
            let right = float_value(builder, rhs_bits);
            let result = builder.ins().fadd(left, right);
            let result_bits = bit_bits(builder, result);
            set_reg(builder, vars, *dst, result_bits)?;
        }
        Inst::SubF { dst, lhs, rhs } => {
            let lhs_bits = reg_value(builder, vars, *lhs)?;
            let left = float_value(builder, lhs_bits);
            let rhs_bits = reg_value(builder, vars, *rhs)?;
            let right = float_value(builder, rhs_bits);
            let result = builder.ins().fsub(left, right);
            let result_bits = bit_bits(builder, result);
            set_reg(builder, vars, *dst, result_bits)?;
        }
        Inst::MulF { dst, lhs, rhs } => {
            let lhs_bits = reg_value(builder, vars, *lhs)?;
            let left = float_value(builder, lhs_bits);
            let rhs_bits = reg_value(builder, vars, *rhs)?;
            let right = float_value(builder, rhs_bits);
            let result = builder.ins().fmul(left, right);
            let result_bits = bit_bits(builder, result);
            set_reg(builder, vars, *dst, result_bits)?;
        }
        Inst::DivF { dst, lhs, rhs } => {
            let lhs_bits = reg_value(builder, vars, *lhs)?;
            let left = float_value(builder, lhs_bits);
            let rhs_bits = reg_value(builder, vars, *rhs)?;
            let right = float_value(builder, rhs_bits);
            let result = builder.ins().fdiv(left, right);
            let result_bits = bit_bits(builder, result);
            set_reg(builder, vars, *dst, result_bits)?;
        }
        Inst::NegF { dst, src } => {
            let bits = reg_value(builder, vars, *src)?;
            let value = float_value(builder, bits);
            let result = builder.ins().fneg(value);
            let result_bits = bit_bits(builder, result);
            set_reg(builder, vars, *dst, result_bits)?;
        }
        Inst::CmpI { dst, lhs, rhs, op } => {
            let left = reg_value(builder, vars, *lhs)?;
            let right = reg_value(builder, vars, *rhs)?;
            let cmp = builder.ins().icmp(int_condition(*op), left, right);
            let result = bool_to_i64(builder, cmp);
            set_reg(builder, vars, *dst, result)?;
        }
        Inst::CmpImmI { dst, lhs, imm, op } => {
            let left = reg_value(builder, vars, *lhs)?;
            let cmp = builder.ins().icmp_imm_s(int_condition(*op), left, *imm);
            let result = bool_to_i64(builder, cmp);
            set_reg(builder, vars, *dst, result)?;
        }
        Inst::CmpF { dst, lhs, rhs, op } => {
            let lhs_bits = reg_value(builder, vars, *lhs)?;
            let left = float_value(builder, lhs_bits);
            let rhs_bits = reg_value(builder, vars, *rhs)?;
            let right = float_value(builder, rhs_bits);
            let cmp = builder.ins().fcmp(float_condition(*op), left, right);
            let result = bool_to_i64(builder, cmp);
            set_reg(builder, vars, *dst, result)?;
        }
        Inst::CmpBool { dst, lhs, rhs, op } => {
            let left = reg_value(builder, vars, *lhs)?;
            let right = reg_value(builder, vars, *rhs)?;
            let cmp = builder.ins().icmp(int_condition(*op), left, right);
            let result = bool_to_i64(builder, cmp);
            set_reg(builder, vars, *dst, result)?;
        }
        Inst::And { dst, lhs, rhs } => {
            let left = reg_value(builder, vars, *lhs)?;
            let right = reg_value(builder, vars, *rhs)?;
            let result = builder.ins().band(left, right);
            set_reg(builder, vars, *dst, result)?;
        }
        Inst::Or { dst, lhs, rhs } => {
            let left = reg_value(builder, vars, *lhs)?;
            let right = reg_value(builder, vars, *rhs)?;
            let result = builder.ins().bor(left, right);
            set_reg(builder, vars, *dst, result)?;
        }
        Inst::Not { dst, src } => {
            let value = reg_value(builder, vars, *src)?;
            let cmp = builder.ins().icmp_imm_s(IntCC::Equal, value, 0);
            let result = bool_to_i64(builder, cmp);
            set_reg(builder, vars, *dst, result)?;
        }
        Inst::Call { dst, target, args } => {
            let array_slot = make_slot(builder, (args.len().max(1) * 8) as u32);
            let array_addr = builder.ins().stack_addr(i64_ty, array_slot, 0);
            for (index, arg) in args.iter().enumerate() {
                let value = reg_value(builder, vars, *arg)?;
                builder
                    .ins()
                    .store(mem_flags(), value, array_addr, (index * 8) as i32);
            }
            let ctx = builder.use_var(ctx_var);
            let ret_slot = make_slot(builder, 8);
            let ret_addr = builder.ins().stack_addr(i64_ty, ret_slot, 0);
            let (dispatch, id) = match target {
                Callable::Function(id) => (dispatch_call, *id as i64),
                Callable::Host(id) => (dispatch_host, *id as i64),
            };
            let id_value = builder.ins().iconst(i64_ty, id);
            builder
                .ins()
                .call(dispatch, &[ctx, id_value, array_addr, ret_addr]);
            let result = builder.ins().load(i64_ty, mem_flags(), ret_addr, 0);
            set_reg(builder, vars, *dst, result)?;
        }
        Inst::Jump { target } => {
            let target = block_at(blocks, func_id, *target)?;
            builder.ins().jump(target, &[]);
            return Ok(true);
        }
        Inst::JumpIfFalse { cond, target } => {
            let cond = reg_value(builder, vars, *cond)?;
            let target = block_at(blocks, func_id, *target)?;
            let is_true = builder.ins().icmp_imm_s(IntCC::NotEqual, cond, 0);
            builder.ins().brif(is_true, next_block, &[], target, &[]);
            return Ok(true);
        }
        Inst::Return { src } => {
            let value = match src {
                Some(reg) => reg_value(builder, vars, *reg)?,
                None => builder.ins().iconst(i64_ty, 0),
            };
            let ret_addr = builder.use_var(ret_var);
            builder.ins().store(mem_flags(), value, ret_addr, 0);
            builder.ins().return_(&[]);
            return Ok(true);
        }
    }
    Ok(false)
}

fn block_at(blocks: &[Block], func_id: FuncId, target: usize) -> Result<Block, JitError> {
    blocks.get(target).copied().ok_or(JitError::InvalidJump {
        func: func_id,
        target,
    })
}

fn float_value(builder: &mut FunctionBuilder<'_>, bits: ClifValue) -> ClifValue {
    builder.ins().bitcast(types::F64, mem_flags(), bits)
}

fn bit_bits(builder: &mut FunctionBuilder<'_>, value: ClifValue) -> ClifValue {
    builder.ins().bitcast(types::I64, mem_flags(), value)
}

fn int_condition(op: CompareOp) -> IntCC {
    match op {
        CompareOp::Eq => IntCC::Equal,
        CompareOp::Ne => IntCC::NotEqual,
        CompareOp::Lt => IntCC::SignedLessThan,
        CompareOp::Le => IntCC::SignedLessThanOrEqual,
        CompareOp::Gt => IntCC::SignedGreaterThan,
        CompareOp::Ge => IntCC::SignedGreaterThanOrEqual,
    }
}

fn float_condition(op: CompareOp) -> cranelift::codegen::ir::condcodes::FloatCC {
    use cranelift::codegen::ir::condcodes::FloatCC;
    match op {
        CompareOp::Eq => FloatCC::Equal,
        CompareOp::Ne => FloatCC::NotEqual,
        CompareOp::Lt => FloatCC::LessThan,
        CompareOp::Le => FloatCC::LessThanOrEqual,
        CompareOp::Gt => FloatCC::GreaterThan,
        CompareOp::Ge => FloatCC::GreaterThanOrEqual,
    }
}

fn is_eligible(module: &Module, func_id: FuncId) -> bool {
    let Some(function) = module.functions.get(func_id as usize) else {
        return false;
    };
    if function
        .params
        .iter()
        .chain(std::iter::once(&function.return_type))
        .any(|ty| matches!(ty, IrType::Str))
    {
        return false;
    }
    function.code.iter().all(|inst| match inst {
        Inst::LoadConst {
            value: Const::Str(_),
            ..
        } => false,
        Inst::Call { target, .. } => match target {
            Callable::Host(_) => false,
            Callable::Function(id) => module.functions.get(*id as usize).is_some_and(|callee| {
                !callee
                    .params
                    .iter()
                    .chain(std::iter::once(&callee.return_type))
                    .any(|ty| matches!(ty, IrType::Str))
            }),
        },
        _ => true,
    })
}

pub fn encode_value(value: &Value) -> Result<u64, JitError> {
    match value {
        Value::Int(value) => Ok(*value as u64),
        Value::Float(value) => Ok(value.to_bits()),
        Value::Bool(value) => Ok(*value as u64),
        Value::Unit => Ok(0),
        Value::Str(_) => Err(JitError::Cranelift(
            "strings cannot cross the JIT boundary".to_string(),
        )),
    }
}

pub fn decode_value(ty: IrType, bits: u64) -> Value {
    match ty {
        IrType::Int => Value::Int(bits as i64),
        IrType::Float => Value::Float(f64::from_bits(bits)),
        IrType::Bool => Value::Bool(bits != 0),
        IrType::Unit | IrType::Str => Value::Unit,
    }
}

#[allow(unsafe_op_in_unsafe_fn)]
unsafe extern "C" fn dispatch_call(
    context: *mut JitContext,
    func_id: u64,
    args: *const u64,
    ret: *mut u64,
) {
    let ctx = &*context;
    let func_id = func_id as FuncId;
    let code = ctx
        .compiled
        .as_ref()
        .unwrap()
        .lock()
        .unwrap()
        .get(&func_id)
        .copied();
    if let Some(code) = code {
        let compiled: CompiledFn = mem::transmute(code);
        compiled(context, args, ret);
        return;
    }

    let module = &*ctx.module;
    let hosts = std::slice::from_raw_parts(ctx.hosts, ctx.host_count);
    let Some(function) = module.functions.get(func_id as usize) else {
        *ret = 0;
        return;
    };
    let values = function
        .params
        .iter()
        .enumerate()
        .map(|(index, ty)| decode_value(*ty, unsafe { *args.add(index) }))
        .collect();
    let mut interpreter = Interpreter::new(module, hosts, None);
    match interpreter.call(func_id, values) {
        Ok(value) => *ret = encode_value(&value).unwrap_or(0),
        Err(error) => {
            eprintln!("JIT interpreter fallback failed: {error}");
            *ret = 0;
        }
    }
}

#[allow(unsafe_op_in_unsafe_fn)]
unsafe extern "C" fn dispatch_host(
    context: *mut JitContext,
    host_id: u64,
    args: *const u64,
    ret: *mut u64,
) {
    let ctx = &*context;
    let hosts = std::slice::from_raw_parts(ctx.hosts, ctx.host_count);
    let Some(host) = hosts.get(host_id as HostId as usize) else {
        *ret = 0;
        return;
    };
    let values = host
        .decl
        .params
        .iter()
        .enumerate()
        .map(|(index, ty)| match ty {
            crust_frontend::typed::Type::I64 => Value::Int(unsafe { *args.add(index) } as i64),
            crust_frontend::typed::Type::F64 => {
                Value::Float(f64::from_bits(unsafe { *args.add(index) }))
            }
            crust_frontend::typed::Type::Bool => Value::Bool(unsafe { *args.add(index) } != 0),
            _ => Value::Unit,
        })
        .collect::<Vec<_>>();
    match encode_value(&(host.call)(&values)) {
        Ok(bits) => *ret = bits,
        Err(error) => {
            eprintln!("JIT host call failed: {error}");
            *ret = 0;
        }
    }
}
