use thiserror::Error;

use crate::{
    ir::{Callable, CompareOp, FuncId, Inst, Module, Reg},
    jit::JitBridge,
    runtime::HostFn,
    value::Value,
};

#[derive(Debug, Error)]
pub enum RuntimeError {
    #[error("unknown function {0}")]
    UnknownFunction(FuncId),
    #[error("unknown host function {0}")]
    UnknownHost(u32),
    #[error("arity mismatch: expected {expected}, got {found}")]
    ArityMismatch { expected: usize, found: usize },
    #[error("register r{reg} is outside a frame of {nregs} registers")]
    InvalidRegister { reg: Reg, nregs: usize },
    #[error("instruction pointer {pc} is outside a function of {len} instructions")]
    InvalidPc { pc: usize, len: usize },
    #[error("expected int, got {0:?}")]
    ExpectedInt(Value),
    #[error("expected float, got {0:?}")]
    ExpectedFloat(Value),
    #[error("expected bool, got {0:?}")]
    ExpectedBool(Value),
    #[error("division by zero")]
    DivisionByZero,
    #[error("returned from a frame with no caller")]
    MissingCaller,
}

#[derive(Debug)]
struct Frame {
    func_id: FuncId,
    base: usize,
    nregs: usize,
    pc: usize,
    return_dst: Option<Reg>,
}

pub struct Interpreter<'a> {
    module: &'a Module,
    hosts: &'a [HostFn],
    bridge: Option<&'a JitBridge>,
    values: Vec<Value>,
    frames: Vec<Frame>,
}

impl<'a> Interpreter<'a> {
    pub fn new(module: &'a Module, hosts: &'a [HostFn], bridge: Option<&'a JitBridge>) -> Self {
        Self {
            module,
            hosts,
            bridge,
            values: Vec::new(),
            frames: Vec::new(),
        }
    }

    pub fn call(&mut self, func_id: FuncId, args: Vec<Value>) -> Result<Value, RuntimeError> {
        let base_depth = self.frames.len();
        push_frame(
            self.module,
            &mut self.values,
            &mut self.frames,
            func_id,
            args,
        )?;

        let mut result = Value::Unit;
        while self.frames.len() > base_depth {
            if let Some(value) = self.step()? {
                result = value;
            }
        }
        Ok(result)
    }

    fn step(&mut self) -> Result<Option<Value>, RuntimeError> {
        let (func_id, base, nregs, pc) = {
            let frame = self.frames.last().expect("no active frame");
            (frame.func_id, frame.base, frame.nregs, frame.pc)
        };
        let function = &self.module.functions[func_id as usize];
        let inst = function.code.get(pc).ok_or(RuntimeError::InvalidPc {
            pc,
            len: function.code.len(),
        })?;
        self.frames.last_mut().expect("no active frame").pc = pc + 1;

        match inst {
            Inst::LoadConst { dst, value } => {
                write(&mut self.values, base, nregs, *dst, value.to_value())?;
            }
            Inst::Move { dst, src } => {
                let value = read(&self.values, base, nregs, *src)?;
                write(&mut self.values, base, nregs, *dst, value)?;
            }
            Inst::AddI { dst, lhs, rhs } => {
                let left = read_int(&self.values, base, nregs, *lhs)?;
                let right = read_int(&self.values, base, nregs, *rhs)?;
                write(
                    &mut self.values,
                    base,
                    nregs,
                    *dst,
                    Value::Int(left + right),
                )?;
            }
            Inst::AddImm { dst, lhs, imm } => {
                let left = read_int(&self.values, base, nregs, *lhs)?;
                write(&mut self.values, base, nregs, *dst, Value::Int(left + imm))?;
            }
            Inst::SubI { dst, lhs, rhs } => {
                let left = read_int(&self.values, base, nregs, *lhs)?;
                let right = read_int(&self.values, base, nregs, *rhs)?;
                write(
                    &mut self.values,
                    base,
                    nregs,
                    *dst,
                    Value::Int(left - right),
                )?;
            }
            Inst::SubImm { dst, lhs, imm } => {
                let left = read_int(&self.values, base, nregs, *lhs)?;
                write(&mut self.values, base, nregs, *dst, Value::Int(left - imm))?;
            }
            Inst::MulI { dst, lhs, rhs } => {
                let left = read_int(&self.values, base, nregs, *lhs)?;
                let right = read_int(&self.values, base, nregs, *rhs)?;
                write(
                    &mut self.values,
                    base,
                    nregs,
                    *dst,
                    Value::Int(left * right),
                )?;
            }
            Inst::DivI { dst, lhs, rhs } => {
                let left = read_int(&self.values, base, nregs, *lhs)?;
                let right = read_int(&self.values, base, nregs, *rhs)?;
                if right == 0 {
                    return Err(RuntimeError::DivisionByZero);
                }
                write(
                    &mut self.values,
                    base,
                    nregs,
                    *dst,
                    Value::Int(left / right),
                )?;
            }
            Inst::ModI { dst, lhs, rhs } => {
                let left = read_int(&self.values, base, nregs, *lhs)?;
                let right = read_int(&self.values, base, nregs, *rhs)?;
                if right == 0 {
                    return Err(RuntimeError::DivisionByZero);
                }
                write(
                    &mut self.values,
                    base,
                    nregs,
                    *dst,
                    Value::Int(left % right),
                )?;
            }
            Inst::NegI { dst, src } => {
                let value = read_int(&self.values, base, nregs, *src)?;
                write(&mut self.values, base, nregs, *dst, Value::Int(-value))?;
            }
            Inst::AddF { dst, lhs, rhs } => {
                let left = read_float(&self.values, base, nregs, *lhs)?;
                let right = read_float(&self.values, base, nregs, *rhs)?;
                write(
                    &mut self.values,
                    base,
                    nregs,
                    *dst,
                    Value::Float(left + right),
                )?;
            }
            Inst::SubF { dst, lhs, rhs } => {
                let left = read_float(&self.values, base, nregs, *lhs)?;
                let right = read_float(&self.values, base, nregs, *rhs)?;
                write(
                    &mut self.values,
                    base,
                    nregs,
                    *dst,
                    Value::Float(left - right),
                )?;
            }
            Inst::MulF { dst, lhs, rhs } => {
                let left = read_float(&self.values, base, nregs, *lhs)?;
                let right = read_float(&self.values, base, nregs, *rhs)?;
                write(
                    &mut self.values,
                    base,
                    nregs,
                    *dst,
                    Value::Float(left * right),
                )?;
            }
            Inst::DivF { dst, lhs, rhs } => {
                let left = read_float(&self.values, base, nregs, *lhs)?;
                let right = read_float(&self.values, base, nregs, *rhs)?;
                write(
                    &mut self.values,
                    base,
                    nregs,
                    *dst,
                    Value::Float(left / right),
                )?;
            }
            Inst::NegF { dst, src } => {
                let value = read_float(&self.values, base, nregs, *src)?;
                write(&mut self.values, base, nregs, *dst, Value::Float(-value))?;
            }
            Inst::CmpI { dst, lhs, rhs, op } => {
                let left = read_int(&self.values, base, nregs, *lhs)?;
                let right = read_int(&self.values, base, nregs, *rhs)?;
                write(
                    &mut self.values,
                    base,
                    nregs,
                    *dst,
                    Value::Bool(compare_i(*op, left, right)),
                )?;
            }
            Inst::CmpImmI { dst, lhs, imm, op } => {
                let left = read_int(&self.values, base, nregs, *lhs)?;
                write(
                    &mut self.values,
                    base,
                    nregs,
                    *dst,
                    Value::Bool(compare_i(*op, left, *imm)),
                )?;
            }
            Inst::CmpF { dst, lhs, rhs, op } => {
                let left = read_float(&self.values, base, nregs, *lhs)?;
                let right = read_float(&self.values, base, nregs, *rhs)?;
                write(
                    &mut self.values,
                    base,
                    nregs,
                    *dst,
                    Value::Bool(compare_f(*op, left, right)),
                )?;
            }
            Inst::CmpBool { dst, lhs, rhs, op } => {
                let left = read_bool(&self.values, base, nregs, *lhs)?;
                let right = read_bool(&self.values, base, nregs, *rhs)?;
                write(
                    &mut self.values,
                    base,
                    nregs,
                    *dst,
                    Value::Bool(compare_i(*op, left as i64, right as i64)),
                )?;
            }
            Inst::And { dst, lhs, rhs } => {
                let left = read_bool(&self.values, base, nregs, *lhs)?;
                let right = read_bool(&self.values, base, nregs, *rhs)?;
                write(
                    &mut self.values,
                    base,
                    nregs,
                    *dst,
                    Value::Bool(left && right),
                )?;
            }
            Inst::Or { dst, lhs, rhs } => {
                let left = read_bool(&self.values, base, nregs, *lhs)?;
                let right = read_bool(&self.values, base, nregs, *rhs)?;
                write(
                    &mut self.values,
                    base,
                    nregs,
                    *dst,
                    Value::Bool(left || right),
                )?;
            }
            Inst::Not { dst, src } => {
                let value = read_bool(&self.values, base, nregs, *src)?;
                write(&mut self.values, base, nregs, *dst, Value::Bool(!value))?;
            }
            Inst::Call { dst, target, args } => {
                let values = args
                    .iter()
                    .map(|&reg| read(&self.values, base, nregs, reg))
                    .collect::<Result<Vec<_>, _>>()?;
                match target {
                    Callable::Host(id) => {
                        let host = self
                            .hosts
                            .get(*id as usize)
                            .ok_or(RuntimeError::UnknownHost(*id))?;
                        let value = (host.call)(&values);
                        write(&mut self.values, base, nregs, *dst, value)?;
                    }
                    Callable::Function(id) => {
                        let compiled_value = self.bridge.and_then(|bridge| {
                            bridge.note_call(*id);
                            bridge.call_if_compiled(*id, &values)
                        });
                        if let Some(value) = compiled_value {
                            write(&mut self.values, base, nregs, *dst, value)?;
                        } else {
                            push_frame(
                                self.module,
                                &mut self.values,
                                &mut self.frames,
                                *id,
                                values,
                            )?;
                            self.frames.last_mut().expect("no active frame").return_dst =
                                Some(*dst);
                        }
                    }
                }
            }
            Inst::Jump { target } => {
                self.frames.last_mut().expect("no active frame").pc = *target;
            }
            Inst::JumpIfFalse { cond, target } => {
                if !read_bool(&self.values, base, nregs, *cond)? {
                    self.frames.last_mut().expect("no active frame").pc = *target;
                }
            }
            Inst::Return { src } => {
                let frame = self.frames.pop().expect("no active frame");
                let value = match src {
                    Some(reg) => read(&self.values, frame.base, frame.nregs, *reg)?,
                    None => Value::Unit,
                };
                if let Some(dst) = frame.return_dst {
                    let caller = self.frames.last().ok_or(RuntimeError::MissingCaller)?;
                    if dst as usize >= caller.nregs {
                        return Err(RuntimeError::InvalidRegister {
                            reg: dst,
                            nregs: caller.nregs,
                        });
                    }
                    self.values[caller.base + dst as usize] = value.clone();
                }
                self.values.truncate(frame.base);
                return Ok(Some(value));
            }
        }

        Ok(None)
    }
}

fn push_frame(
    module: &Module,
    values: &mut Vec<Value>,
    frames: &mut Vec<Frame>,
    func_id: FuncId,
    args: Vec<Value>,
) -> Result<(), RuntimeError> {
    let function = module
        .functions
        .get(func_id as usize)
        .ok_or(RuntimeError::UnknownFunction(func_id))?;
    if args.len() != function.params.len() {
        return Err(RuntimeError::ArityMismatch {
            expected: function.params.len(),
            found: args.len(),
        });
    }

    let base = values.len();
    let nregs = function.nregs;
    values.resize(base + nregs, Value::Unit);
    values[base..base + args.len()].clone_from_slice(&args);
    frames.push(Frame {
        func_id,
        base,
        nregs,
        pc: 0,
        return_dst: None,
    });
    Ok(())
}

fn read(values: &[Value], base: usize, nregs: usize, reg: Reg) -> Result<Value, RuntimeError> {
    if reg as usize >= nregs {
        return Err(RuntimeError::InvalidRegister { reg, nregs });
    }
    Ok(values[base + reg as usize].clone())
}

fn write(
    values: &mut [Value],
    base: usize,
    nregs: usize,
    reg: Reg,
    value: Value,
) -> Result<(), RuntimeError> {
    if reg as usize >= nregs {
        return Err(RuntimeError::InvalidRegister { reg, nregs });
    }
    values[base + reg as usize] = value;
    Ok(())
}

fn read_int(values: &[Value], base: usize, nregs: usize, reg: Reg) -> Result<i64, RuntimeError> {
    if reg as usize >= nregs {
        return Err(RuntimeError::InvalidRegister { reg, nregs });
    }
    match &values[base + reg as usize] {
        Value::Int(value) => Ok(*value),
        other => Err(RuntimeError::ExpectedInt(other.clone())),
    }
}

fn read_float(values: &[Value], base: usize, nregs: usize, reg: Reg) -> Result<f64, RuntimeError> {
    if reg as usize >= nregs {
        return Err(RuntimeError::InvalidRegister { reg, nregs });
    }
    match &values[base + reg as usize] {
        Value::Float(value) => Ok(*value),
        other => Err(RuntimeError::ExpectedFloat(other.clone())),
    }
}

fn read_bool(values: &[Value], base: usize, nregs: usize, reg: Reg) -> Result<bool, RuntimeError> {
    if reg as usize >= nregs {
        return Err(RuntimeError::InvalidRegister { reg, nregs });
    }
    match &values[base + reg as usize] {
        Value::Bool(value) => Ok(*value),
        other => Err(RuntimeError::ExpectedBool(other.clone())),
    }
}

fn compare_i(op: CompareOp, left: i64, right: i64) -> bool {
    match op {
        CompareOp::Eq => left == right,
        CompareOp::Ne => left != right,
        CompareOp::Lt => left < right,
        CompareOp::Le => left <= right,
        CompareOp::Gt => left > right,
        CompareOp::Ge => left >= right,
    }
}

fn compare_f(op: CompareOp, left: f64, right: f64) -> bool {
    match op {
        CompareOp::Eq => left == right,
        CompareOp::Ne => left != right,
        CompareOp::Lt => left < right,
        CompareOp::Le => left <= right,
        CompareOp::Gt => left > right,
        CompareOp::Ge => left >= right,
    }
}
