use std::collections::HashMap;

use crust_frontend::{
    ast::{BinOp, LiteralValue, UnaryOp},
    typed::{HostFnDecl, Type as FrontendType, TypedAstNode, TypedAstNodeId, TypedParameter},
    typed_arena::TypedAstArena,
};

use crate::ir::{Callable, CompareOp, Const, FuncId, Function, HostId, Inst, Module, Reg, Type};

fn lower_type(ty: &FrontendType) -> Type {
    match ty {
        FrontendType::I32 | FrontendType::I64 => Type::Int,
        FrontendType::F32 | FrontendType::F64 => Type::Float,
        FrontendType::Bool => Type::Bool,
        FrontendType::String => Type::Str,
        _ => Type::Unit,
    }
}

fn lower_const(value: &LiteralValue) -> Const {
    match value {
        LiteralValue::Int(value) => Const::Int(*value),
        LiteralValue::Float(value) => Const::Float(*value),
        LiteralValue::Bool(value) => Const::Bool(*value),
        LiteralValue::RawString(value) => Const::Str(value.clone()),
    }
}

fn compare_op(op: &BinOp) -> Option<CompareOp> {
    match op {
        BinOp::Equal => Some(CompareOp::Eq),
        BinOp::NotEqual => Some(CompareOp::Ne),
        BinOp::Lesser => Some(CompareOp::Lt),
        BinOp::LesserEqual => Some(CompareOp::Le),
        BinOp::Greater => Some(CompareOp::Gt),
        BinOp::GreaterEqual => Some(CompareOp::Ge),
        _ => None,
    }
}

#[derive(Debug)]
struct FunctionBuilder {
    name: String,
    params: Vec<Type>,
    return_type: Type,
    nregs: usize,
    constants: Vec<Const>,
    code: Vec<Inst>,
    scopes: Vec<HashMap<String, Reg>>,
}

impl FunctionBuilder {
    fn new(name: String, params: Vec<Type>, return_type: Type) -> Self {
        let nregs = params.len();
        Self {
            name,
            params,
            return_type,
            nregs,
            constants: Vec::new(),
            code: Vec::new(),
            scopes: vec![HashMap::new()],
        }
    }

    fn alloc_reg(&mut self) -> Reg {
        let reg = self.nregs as Reg;
        self.nregs += 1;
        reg
    }

    fn declare(&mut self, name: String, reg: Reg) {
        self.scopes
            .last_mut()
            .expect("no active scope")
            .insert(name, reg);
    }

    fn resolve(&self, name: &str) -> Reg {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.get(name).copied())
            .unwrap_or_else(|| panic!("undeclared variable: {name}"))
    }

    fn emit(&mut self, inst: Inst) {
        self.code.push(inst);
    }

    fn emit_const(&mut self, value: Const) -> Reg {
        self.constants.push(value.clone());
        let dst = self.alloc_reg();
        self.emit(Inst::LoadConst { dst, value });
        dst
    }

    fn patch_jump(&mut self, index: usize, target: usize) {
        match &mut self.code[index] {
            Inst::Jump { target: slot } | Inst::JumpIfFalse { target: slot, .. } => *slot = target,
            _ => panic!("cannot patch non-jump instruction"),
        }
    }

    fn finish(self) -> Function {
        Function {
            name: self.name,
            params: self.params,
            return_type: self.return_type,
            nregs: self.nregs,
            constants: self.constants,
            code: self.code,
        }
    }
}

pub struct CodeGen {
    typed_ast: TypedAstArena,
    host_decls: Vec<HostFnDecl>,
}

impl CodeGen {
    pub fn new(typed_ast: TypedAstArena, host_decls: Vec<HostFnDecl>) -> Self {
        Self {
            typed_ast,
            host_decls,
        }
    }

    pub fn generate(&self, root_id: TypedAstNodeId) -> Module {
        let host_ids = self
            .host_decls
            .iter()
            .enumerate()
            .map(|(id, decl)| (decl.name.clone(), id as HostId))
            .collect();
        let mut generator = Generator {
            typed_ast: &self.typed_ast,
            function_ids: HashMap::new(),
            host_ids,
        };
        generator.generate_module(root_id)
    }
}

struct Generator<'a> {
    typed_ast: &'a TypedAstArena,
    function_ids: HashMap<String, FuncId>,
    host_ids: HashMap<String, HostId>,
}

impl Generator<'_> {
    fn generate_module(&mut self, root_id: TypedAstNodeId) -> Module {
        let root = self.typed_ast.get(root_id).clone();
        let statements = match &root {
            TypedAstNode::Program { statements } => statements.clone(),
            _ => Vec::new(),
        };

        let mut builders: Vec<FunctionBuilder> = Vec::new();
        let mut bodies: Vec<Option<TypedAstNodeId>> = Vec::new();
        let mut top_level: Vec<TypedAstNodeId> = Vec::new();

        if !matches!(root, TypedAstNode::Program { .. }) {
            top_level.push(root_id);
        }

        for stmt_id in &statements {
            match self.typed_ast.get(*stmt_id) {
                TypedAstNode::FunctionDefinition {
                    name,
                    parameters,
                    return_type,
                    ..
                } => {
                    let params: Vec<Type> = parameters.iter().map(lower_param).collect();
                    let mut builder =
                        FunctionBuilder::new(name.clone(), params, lower_type(return_type));
                    for (reg, param) in parameters.iter().enumerate() {
                        builder.declare(param.name.clone(), reg as Reg);
                    }
                    let id = builders.len() as FuncId;
                    self.function_ids.insert(name.clone(), id);
                    builders.push(builder);
                    bodies.push(Some(*stmt_id));
                }
                _ => top_level.push(*stmt_id),
            }
        }

        let entry = if let Some(id) = self.function_ids.get("main").copied() {
            id
        } else {
            let id = builders.len() as FuncId;
            self.function_ids.insert("main".to_string(), id);
            builders.push(FunctionBuilder::new(
                "main".to_string(),
                Vec::new(),
                Type::Unit,
            ));
            bodies.push(None);
            id
        };

        for index in 0..builders.len() {
            let mut builder = std::mem::replace(
                &mut builders[index],
                FunctionBuilder::new(String::new(), Vec::new(), Type::Unit),
            );
            match bodies[index] {
                Some(node_id) => {
                    let body = match self.typed_ast.get(node_id) {
                        TypedAstNode::FunctionDefinition { body, .. } => *body,
                        _ => unreachable!("function builder without a function definition"),
                    };
                    self.generate_node(body, &mut builder);
                }
                None => {
                    for stmt_id in &top_level {
                        self.generate_node(*stmt_id, &mut builder);
                    }
                }
            }
            append_default_return(&mut builder);
            builders[index] = builder;
        }

        let mut host_names = vec![String::new(); self.host_ids.len()];
        for (name, id) in &self.host_ids {
            host_names[*id as usize] = name.clone();
        }

        Module {
            functions: builders.into_iter().map(FunctionBuilder::finish).collect(),
            entry,
            host_names,
        }
    }

    fn generate_node(&mut self, id: TypedAstNodeId, builder: &mut FunctionBuilder) -> Reg {
        let node = self.typed_ast.get(id).clone();
        match node {
            TypedAstNode::Literal { value, .. } => builder.emit_const(lower_const(&value)),
            TypedAstNode::Identifier { name, .. } => builder.resolve(&name),
            TypedAstNode::Comment(_) => 0,
            TypedAstNode::Program { statements } => {
                for stmt_id in statements {
                    self.generate_node(stmt_id, builder);
                }
                0
            }
            TypedAstNode::Block { statements } => {
                builder.scopes.push(HashMap::new());
                for stmt_id in statements {
                    self.generate_node(stmt_id, builder);
                }
                builder.scopes.pop();
                0
            }
            TypedAstNode::LetStatement {
                identifier, value, ..
            } => {
                let src = self.generate_node(value, builder);
                let dst = builder.alloc_reg();
                builder.emit(Inst::Move { dst, src });
                builder.declare(identifier, dst);
                dst
            }
            TypedAstNode::BinaryExpression {
                left,
                op,
                right,
                result_type,
            } => {
                let lhs = self.generate_node(left, builder);
                let dst = builder.alloc_reg();
                let operand_type = self
                    .typed_ast
                    .get(left)
                    .get_type()
                    .map(lower_type)
                    .unwrap_or_else(|| lower_type(&result_type));
                let immediate = match self.typed_ast.get(right) {
                    TypedAstNode::Literal {
                        value: LiteralValue::Int(value),
                        ..
                    } => Some(*value),
                    _ => None,
                };

                let emitted = immediate
                    .is_some_and(|imm| emit_binary_imm(builder, &op, operand_type, dst, lhs, imm));
                if !emitted {
                    let rhs = self.generate_node(right, builder);
                    emit_binary(builder, &op, operand_type, dst, lhs, rhs);
                }
                dst
            }
            TypedAstNode::UnaryExpression {
                op,
                expression,
                result_type,
            } => {
                let src = self.generate_node(expression, builder);
                match op {
                    UnaryOp::Negate => {
                        let dst = builder.alloc_reg();
                        match lower_type(&result_type) {
                            Type::Int => builder.emit(Inst::NegI { dst, src }),
                            Type::Float => builder.emit(Inst::NegF { dst, src }),
                            ty => panic!("cannot negate {ty:?}"),
                        }
                        dst
                    }
                    UnaryOp::Not => {
                        let dst = builder.alloc_reg();
                        builder.emit(Inst::Not { dst, src });
                        dst
                    }
                    UnaryOp::PreIncrement
                    | UnaryOp::PostIncrement
                    | UnaryOp::PreDecrement
                    | UnaryOp::PostDecrement => {
                        let delta = if matches!(op, UnaryOp::PreIncrement | UnaryOp::PostIncrement)
                        {
                            1
                        } else {
                            -1
                        };
                        let old = builder.alloc_reg();
                        builder.emit(Inst::Move { dst: old, src });
                        builder.emit(Inst::AddImm {
                            dst: src,
                            lhs: src,
                            imm: delta,
                        });
                        if matches!(op, UnaryOp::PostIncrement | UnaryOp::PostDecrement) {
                            old
                        } else {
                            src
                        }
                    }
                }
            }
            TypedAstNode::IfStatement {
                condition,
                then_block,
                else_block,
            } => {
                let cond = self.generate_node(condition, builder);
                let false_jump = builder.code.len();
                builder.emit(Inst::JumpIfFalse {
                    cond,
                    target: usize::MAX,
                });
                self.generate_node(then_block, builder);
                if let Some(else_block) = else_block {
                    let end_jump = builder.code.len();
                    builder.emit(Inst::Jump { target: usize::MAX });
                    builder.patch_jump(false_jump, builder.code.len());
                    self.generate_node(else_block, builder);
                    builder.patch_jump(end_jump, builder.code.len());
                } else {
                    builder.patch_jump(false_jump, builder.code.len());
                }
                0
            }
            TypedAstNode::ForLoop {
                initializer,
                condition,
                increment,
                block,
            } => {
                self.generate_node(initializer, builder);
                let condition_start = builder.code.len();
                let cond = self.generate_node(condition, builder);
                let exit_jump = builder.code.len();
                builder.emit(Inst::JumpIfFalse {
                    cond,
                    target: usize::MAX,
                });
                self.generate_node(block, builder);
                self.generate_node(increment, builder);
                builder.emit(Inst::Jump {
                    target: condition_start,
                });
                builder.patch_jump(exit_jump, builder.code.len());
                0
            }
            TypedAstNode::ReturnStatement { value, .. } => {
                let src = self.generate_node(value, builder);
                builder.emit(Inst::Return { src: Some(src) });
                src
            }
            TypedAstNode::FunctionCall {
                callee,
                arguments,
                return_type,
            } => {
                let args = arguments
                    .into_iter()
                    .map(|id| self.generate_node(id, builder))
                    .collect();
                let target = if let Some(id) = self.function_ids.get(&callee) {
                    Callable::Function(*id)
                } else if let Some(id) = self.host_ids.get(&callee) {
                    Callable::Host(*id)
                } else {
                    panic!("unknown callable: {callee}");
                };
                let dst = builder.alloc_reg();
                builder.emit(Inst::Call { dst, target, args });
                let _ = return_type;
                dst
            }
            TypedAstNode::FunctionDefinition { .. } => 0,
        }
    }
}

fn lower_param(param: &TypedParameter) -> Type {
    lower_type(&param.param_type)
}

fn emit_binary_imm(
    builder: &mut FunctionBuilder,
    op: &BinOp,
    ty: Type,
    dst: Reg,
    lhs: Reg,
    imm: i64,
) -> bool {
    match (op, ty) {
        (BinOp::Add, Type::Int) => builder.emit(Inst::AddImm { dst, lhs, imm }),
        (BinOp::Sub, Type::Int) => builder.emit(Inst::SubImm { dst, lhs, imm }),
        (op, Type::Int) => match compare_op(op) {
            Some(cmp) => builder.emit(Inst::CmpImmI {
                dst,
                lhs,
                imm,
                op: cmp,
            }),
            None => return false,
        },
        _ => return false,
    }
    true
}

fn emit_binary(builder: &mut FunctionBuilder, op: &BinOp, ty: Type, dst: Reg, lhs: Reg, rhs: Reg) {
    match (op, ty) {
        (BinOp::Add, Type::Int) => builder.emit(Inst::AddI { dst, lhs, rhs }),
        (BinOp::Sub, Type::Int) => builder.emit(Inst::SubI { dst, lhs, rhs }),
        (BinOp::Multiply, Type::Int) => builder.emit(Inst::MulI { dst, lhs, rhs }),
        (BinOp::Divide, Type::Int) => builder.emit(Inst::DivI { dst, lhs, rhs }),
        (BinOp::Modulo, Type::Int) => builder.emit(Inst::ModI { dst, lhs, rhs }),
        (BinOp::Add, Type::Float) => builder.emit(Inst::AddF { dst, lhs, rhs }),
        (BinOp::Sub, Type::Float) => builder.emit(Inst::SubF { dst, lhs, rhs }),
        (BinOp::Multiply, Type::Float) => builder.emit(Inst::MulF { dst, lhs, rhs }),
        (BinOp::Divide, Type::Float) => builder.emit(Inst::DivF { dst, lhs, rhs }),
        (BinOp::And, Type::Bool) => builder.emit(Inst::And { dst, lhs, rhs }),
        (BinOp::Or, Type::Bool) => builder.emit(Inst::Or { dst, lhs, rhs }),
        (op, ty) => {
            let cmp =
                compare_op(op).unwrap_or_else(|| panic!("unsupported binary op: {op:?} on {ty:?}"));
            match ty {
                Type::Int => builder.emit(Inst::CmpI {
                    dst,
                    lhs,
                    rhs,
                    op: cmp,
                }),
                Type::Float => builder.emit(Inst::CmpF {
                    dst,
                    lhs,
                    rhs,
                    op: cmp,
                }),
                Type::Bool => builder.emit(Inst::CmpBool {
                    dst,
                    lhs,
                    rhs,
                    op: cmp,
                }),
                _ => panic!("unsupported comparison on {ty:?}"),
            }
        }
    }
}

fn append_default_return(builder: &mut FunctionBuilder) {
    if matches!(builder.code.last(), Some(Inst::Return { .. })) {
        return;
    }
    match builder.return_type {
        Type::Unit => builder.emit(Inst::Return { src: None }),
        Type::Int => {
            let reg = builder.emit_const(Const::Int(0));
            builder.emit(Inst::Return { src: Some(reg) });
        }
        Type::Float => {
            let reg = builder.emit_const(Const::Float(0.0));
            builder.emit(Inst::Return { src: Some(reg) });
        }
        Type::Bool => {
            let reg = builder.emit_const(Const::Bool(false));
            builder.emit(Inst::Return { src: Some(reg) });
        }
        Type::Str => {
            let reg = builder.emit_const(Const::Str(String::new()));
            builder.emit(Inst::Return { src: Some(reg) });
        }
    }
}
