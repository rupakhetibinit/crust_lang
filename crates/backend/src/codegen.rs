use crust_frontend::{
    ast::BinOp,
    typed::{Type, TypedAstNode},
    typed_arena::TypedAstArena,
};

use crate::chunk::Chunk;

pub struct CodeGen {
    typed_ast: TypedAstArena,
}

#[allow(unused)]
impl CodeGen {
    pub fn new(typed_arena: TypedAstArena) -> Self {
        Self {
            typed_ast: typed_arena,
        }
    }
    pub fn generate(&mut self, root_id: usize) -> Chunk {
        let mut chunk = Chunk::new();
        if let TypedAstNode::Program { statements } = self.typed_ast.get(root_id).clone() {
            for id in &statements {
                if let TypedAstNode::FunctionDefinition { name, .. } = self.typed_ast.get(*id) {
                    if name == "main" {
                        self.generate_node(*id, &mut chunk);
                    }
                }
            }
            chunk.emit(OpCode::Halt);
            for id in statements {
                if let TypedAstNode::FunctionDefinition {
                    name,
                    parameters,
                    body,
                    ..
                } = self.typed_ast.get(id).clone()
                {
                    if name != "main" {
                        chunk.functions.insert(name, chunk.code.len());
                        chunk.locals.clear();
                        for parameter in parameters {
                            chunk.declare_variable(parameter.name);
                        }
                        self.generate_node(body, &mut chunk);
                        let zero = chunk.add_constant(crust_frontend::ast::LiteralValue::Int(0));
                        chunk.emit(OpCode::LoadConst(zero));
                        chunk.emit(OpCode::Return);
                    }
                }
            }
            chunk.resolve_calls();
        } else {
            self.generate_node(root_id, &mut chunk);
            chunk.emit(OpCode::Halt);
        }
        chunk
    }

    pub fn generate_node(&mut self, id: usize, chunk: &mut Chunk) {
        let root = self.typed_ast.get(id).clone();

        match root {
            TypedAstNode::BinaryExpression {
                left,
                op,
                right,
                result_type,
            } => {
                self.generate_node(left, chunk);
                self.generate_node(right, chunk);
                let operand_type = self.typed_ast.get(left).get_type().cloned();
                match (op.clone(), operand_type, result_type.clone()) {
                    (BinOp::Lesser, Some(Type::I64 | Type::I32), Type::Bool) => {
                        chunk.emit(OpCode::LessI64)
                    }
                    (BinOp::LesserEqual, Some(Type::I64 | Type::I32), Type::Bool) => {
                        chunk.emit(OpCode::LessEqualI64)
                    }
                    (BinOp::Greater, Some(Type::I64 | Type::I32), Type::Bool) => {
                        chunk.emit(OpCode::GreaterI64)
                    }
                    (BinOp::GreaterEqual, Some(Type::I64 | Type::I32), Type::Bool) => {
                        chunk.emit(OpCode::GreaterEqualI64)
                    }
                    (BinOp::Add, _, Type::I64 | Type::I32) => chunk.emit(OpCode::AddI64),
                    (BinOp::Add, _, Type::F64) => chunk.emit(OpCode::AddF64),
                    (BinOp::Sub, _, Type::I64 | Type::I32) => chunk.emit(OpCode::SubI64),
                    (BinOp::Equal, Some(Type::I64 | Type::I32), Type::Bool) => {
                        chunk.emit(OpCode::EqualI64)
                    }
                    (BinOp::Multiply, _, Type::I64) => chunk.emit(OpCode::MulI64),
                    (BinOp::Multiply, _, Type::F64 | Type::F32) => chunk.emit(OpCode::MulF64),
                    (BinOp::Divide, _, Type::I64) => chunk.emit(OpCode::DivI64),
                    (BinOp::Divide, _, Type::F64 | Type::F32) => chunk.emit(OpCode::DivF64),
                    _ => todo!("unsupported binary op: {:?} {:?}", op, result_type),
                }
            }
            TypedAstNode::Literal {
                value,
                literal_type,
            } => {
                let const_idx = chunk.add_constant(value.clone());
                chunk.emit(OpCode::LoadConst(const_idx));
            }
            TypedAstNode::Program { statements } => {
                for stmt_id in statements {
                    self.generate_node(stmt_id, chunk);
                }
                chunk.emit(OpCode::Halt);
            }
            TypedAstNode::LetStatement {
                identifier,
                declared_type,
                value,
            } => {
                self.generate_node(value, chunk);
                let slot = chunk.declare_variable(identifier.clone());
                chunk.emit(OpCode::StoreLocal(slot));
            }
            TypedAstNode::ReturnStatement { value, return_type } => {
                self.generate_node(value, chunk);
                chunk.emit(OpCode::Return);
            }
            TypedAstNode::IfStatement {
                condition,
                then_block,
                else_block,
            } => {
                self.generate_node(condition, chunk);
                let false_jump = chunk.code.len();
                chunk.emit(OpCode::JumpIfFalse(usize::MAX));
                self.generate_node(then_block, chunk);
                if let Some(else_block) = else_block {
                    let end_jump = chunk.code.len();
                    chunk.emit(OpCode::Jump(usize::MAX));
                    chunk.patch_jump(false_jump, chunk.code.len());
                    self.generate_node(else_block, chunk);
                    chunk.patch_jump(end_jump, chunk.code.len());
                } else {
                    chunk.patch_jump(false_jump, chunk.code.len());
                }
            }
            TypedAstNode::FunctionDefinition {
                name,
                parameters,
                return_type,
                body,
                function_type,
            } => {
                if &name == "main" {
                    chunk.locals.clear();
                    for parameter in parameters {
                        chunk.declare_variable(parameter.name);
                    }
                    self.generate_node(body, chunk);
                }
            }
            TypedAstNode::Block { statements } => {
                for stmt in statements {
                    self.generate_node(stmt, chunk);
                }
            }
            TypedAstNode::Identifier {
                name,
                resolved_type,
            } => {
                let slot = chunk.resolve_variable(&name);
                chunk.emit(OpCode::LoadLocal(slot));
            }
            TypedAstNode::Comment(_) => {}
            TypedAstNode::ForLoop {
                initializer,
                condition,
                increment,
                block,
            } => {
                self.generate_node(initializer, chunk);
                let condition_start = chunk.code.len();
                self.generate_node(condition, chunk);
                let exit_jump = chunk.code.len();
                chunk.emit(OpCode::JumpIfFalse(usize::MAX));
                self.generate_node(block, chunk);
                self.generate_node(increment, chunk);
                chunk.emit(OpCode::Jump(condition_start));
                chunk.patch_jump(exit_jump, chunk.code.len());
            }
            TypedAstNode::UnaryExpression {
                op,
                expression,
                result_type,
            } => {
                let slot = match self.typed_ast.get(expression) {
                    TypedAstNode::Identifier { name, .. } => chunk.resolve_variable(name),
                    _ => panic!("increment operand must be a local variable"),
                };
                match op {
                    crust_frontend::ast::UnaryOp::PreIncrement
                    | crust_frontend::ast::UnaryOp::PostIncrement => {
                        chunk.emit(OpCode::IncrementLocal(slot))
                    }
                    crust_frontend::ast::UnaryOp::PreDecrement
                    | crust_frontend::ast::UnaryOp::PostDecrement => {
                        chunk.emit(OpCode::DecrementLocal(slot))
                    }
                    _ => todo!("unsupported unary operation: {:?}", op),
                }
            }
            TypedAstNode::FunctionCall {
                callee,
                arguments,
                return_type,
            } => {
                if callee == "print" {
                    for argument in &arguments {
                        self.generate_node(*argument, chunk);
                    }
                    chunk.emit(OpCode::Print(arguments.len()));
                } else {
                    let argument_count = arguments.len();
                    for argument in arguments {
                        self.generate_node(argument, chunk);
                    }
                    chunk.emit_call(callee, argument_count);
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum ConstantType {
    Int(i64),
    Float(f64),
    String(String),
}

#[derive(Debug, Clone)]
pub enum OpCode {
    LoadConst(usize),
    LoadLocal(usize),
    StoreLocal(usize),
    AddI64,
    AddF64,
    SubI64,
    EqualI64,
    LessI64,
    LessEqualI64,
    GreaterI64,
    GreaterEqualI64,
    MulI64,
    MulF64,
    NegI64,
    DivF64,
    DivI64,
    Call(usize, usize),
    Return,
    Halt,
    Print(usize),
    IncrementLocal(usize),
    DecrementLocal(usize),
    JumpIfFalse(usize),
    Jump(usize),
}
