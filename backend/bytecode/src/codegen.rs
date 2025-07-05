use ast::BinOp;
use typecheck::{Type, TypedAstArena, TypedAstNode};

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
        self.generate_node(root_id, &mut chunk);
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
                match (op.clone(), result_type.clone()) {
                    (BinOp::Add, Type::I64) => chunk.emit(OpCode::AddI64),
                    (BinOp::Add, Type::I32) => chunk.emit(OpCode::AddI64),
                    (BinOp::Add, Type::F64) => chunk.emit(OpCode::AddF64),
                    (BinOp::Multiply, Type::I64) => chunk.emit(OpCode::MulI64),
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
            TypedAstNode::FunctionDefinition {
                name,
                parameters,
                return_type,
                body,
                function_type,
            } => {
                if &name == "main" {
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
            } => todo!(),
            TypedAstNode::UnaryExpression {
                op,
                expression,
                result_type,
            } => todo!(),
            TypedAstNode::FunctionCall {
                callee,
                arguments,
                return_type,
            } => {
                if callee == "print" {
                    let arguments = arguments.get(0).unwrap();
                    self.generate_node(*arguments, chunk);
                    chunk.emit(OpCode::Print);
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
    MulI64,
    NegI64,
    Call(String),
    Return,
    Halt,
    Print,
}
