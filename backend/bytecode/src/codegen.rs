use ast::{BinOp, LiteralValue};
use tracing::{info, instrument};
use typecheck::{Type, TypedAstArena, TypedAstNode};

use crate::chunk::Chunk;

pub struct CodeGen {
    typed_ast: TypedAstArena,
}

fn literal_value_to_constant_type(value: LiteralValue) -> ConstantType {
    match value {
        LiteralValue::Int(i) => ConstantType::Int(i),
        LiteralValue::Float(f) => ConstantType::Float(f),
        LiteralValue::Bool(b) => ConstantType::Bool(b),
        LiteralValue::RawString(s) => ConstantType::String(s),
    }
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
        info!("Starting code generation for root_id: {}", root_id);

        // First pass: collect all function definitions
        self.collect_functions(root_id, &mut chunk);

        // Second pass: find and call main function
        let mut main_idx = None;
        for (i, constant) in chunk.constants.iter().enumerate() {
            if let ConstantType::Function(func_obj) = constant {
                if func_obj.name == "main" {
                    main_idx = Some(i);
                    break;
                }
            }
        }

        if let Some(idx) = main_idx {
            info!("Found main function at index {}", idx);
            chunk.emit(OpCode::Call(0, idx));
        } else {
            info!("No main function found, generating program directly");
            self.generate_node(root_id, &mut chunk);
        }

        chunk.emit(OpCode::Halt);

        info!(
            "Finished code generation. Chunk has {} instructions and {} constants",
            chunk.code.len(),
            chunk.constants.len()
        );

        chunk
    }

    #[instrument(skip(self, chunk))]
    fn collect_functions(&mut self, id: usize, chunk: &mut Chunk) {
        let node = self.typed_ast.get(id).clone();

        match node {
            TypedAstNode::Program { statements } => {
                info!(
                    "Collecting functions from program with {} statements",
                    statements.len()
                );
                for stmt_id in statements {
                    self.collect_functions(stmt_id, chunk);
                }
            }
            TypedAstNode::FunctionDefinition {
                name,
                parameters,
                return_type,
                body,
                function_type,
            } => {
                info!(
                    "Collecting function definition: {} with {} parameters",
                    name,
                    parameters.len()
                );
                let mut func_chunk = Chunk::new();

                // Copy over existing global constants (like other functions)
                func_chunk.constants.extend(chunk.constants.clone());

                // Set up parameters as local variables
                for param in parameters.clone() {
                    func_chunk.declare_variable(param.name);
                }

                self.generate_node(body, &mut func_chunk);

                // Ensure function returns properly
                if !func_chunk.code.ends_with(&[OpCode::Return]) {
                    // For void functions, just return
                    func_chunk.emit(OpCode::Return);
                }

                let func_idx = chunk.add_constant(ConstantType::Function(FunctionObj {
                    name: name.clone(),
                    arity: parameters.len(),
                    chunk: func_chunk,
                }));

                info!("Function {} collected with index {}", name, func_idx);
            }
            _ => {}
        }
    }

    #[instrument(skip(self, chunk), fields(node_type = %std::any::type_name::<TypedAstNode>()))]
    pub fn generate_node(&mut self, id: usize, chunk: &mut Chunk) {
        let root = self.typed_ast.get(id).clone();

        info!(
            "Processing node {}: {:?}",
            id,
            std::mem::discriminant(&root)
        );

        match root {
            TypedAstNode::BinaryExpression {
                left,
                op,
                right,
                result_type,
            } => {
                info!(
                    "Processing BinaryExpression: {:?} with type {:?}",
                    op, result_type
                );
                info!("Left: {}, Right: {}", left, right);

                self.generate_node(left, chunk);
                self.generate_node(right, chunk);
                match (op.clone(), result_type.clone()) {
                    (BinOp::Add, Type::I64) => chunk.emit(OpCode::AddI64),
                    (BinOp::Add, Type::I32) => chunk.emit(OpCode::AddI64),
                    (BinOp::Add, Type::F64) => chunk.emit(OpCode::AddF64),
                    (BinOp::Multiply, Type::I64) => chunk.emit(OpCode::MulI64),
                    (BinOp::Multiply, Type::F64) => chunk.emit(OpCode::MulF64),
                    _ => todo!("unsupported binary op: {:?} {:?}", op, result_type),
                }
            }
            TypedAstNode::Literal {
                value,
                literal_type,
            } => {
                info!(
                    "Processing Literal: {:?} with type {:?}",
                    value, literal_type
                );
                let const_idx = chunk.add_constant(literal_value_to_constant_type(value));
                chunk.emit(OpCode::LoadConst(const_idx));
            }
            TypedAstNode::Program { statements } => {
                info!("Processing Program with {} statements", statements.len());
                for (i, stmt_id) in statements.iter().enumerate() {
                    info!("Processing statement {} (id: {})", i, stmt_id);
                    self.generate_node(*stmt_id, chunk);
                }
            }
            TypedAstNode::LetStatement {
                identifier,
                declared_type,
                value,
            } => {
                info!(
                    "Processing LetStatement: {} with type {:?}",
                    identifier, declared_type
                );
                let slot = chunk.declare_variable(identifier.clone());
                self.generate_node(value, chunk);
                chunk.emit(OpCode::StoreLocal(slot));
            }
            TypedAstNode::ReturnStatement { value, return_type } => {
                info!("Processing ReturnStatement with type {:?}", return_type);
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
                info!(
                    "Processing FunctionDefinition: {} with {} parameters and return type {:?}",
                    name,
                    parameters.len(),
                    return_type
                );
                info!("Function type: {:?}", function_type);
            }
            TypedAstNode::Block { statements } => {
                info!("Processing Block with {} statements", statements.len());
                for stmt in statements {
                    self.generate_node(stmt, chunk);
                }
            }
            TypedAstNode::Identifier {
                name,
                resolved_type,
            } => {
                info!(
                    "Processing Identifier: {} with type {:?}",
                    name, resolved_type
                );
                let slot = chunk.resolve_variable(&name);
                chunk.emit(OpCode::LoadLocal(slot));
            }
            TypedAstNode::Comment(_) => {
                info!("Processing Comment (no code generation needed)");
            }
            TypedAstNode::ForLoop {
                initializer,
                condition,
                increment,
                block,
            } => {
                info!("Processing ForLoop with initializer, condition, increment and block");
                todo!()
            }
            TypedAstNode::UnaryExpression {
                op,
                expression,
                result_type,
            } => {
                info!(
                    "Processing UnaryExpression: {:?} with type {:?}",
                    op, result_type
                );
                todo!()
            }
            TypedAstNode::FunctionCall {
                callee,
                arguments,
                return_type,
            } => {
                info!(
                    "Processing FunctionCall: {} with {} arguments and return type {:?}",
                    callee,
                    arguments.len(),
                    return_type
                );
                if callee == "print" {
                    if let Some(&arg_id) = arguments.get(0) {
                        self.generate_node(arg_id, chunk);
                        chunk.emit(OpCode::Print);
                    }
                } else {
                    // Generate arguments
                    for &arg_id in arguments.iter() {
                        self.generate_node(arg_id, chunk);
                    }

                    // Find the function in constants
                    let mut func_idx = None;
                    for (i, constant) in chunk.constants.iter().enumerate() {
                        if let ConstantType::Function(func_obj) = constant {
                            if func_obj.name == callee {
                                func_idx = Some(i);
                                break;
                            }
                        }
                    }

                    if let Some(idx) = func_idx {
                        info!("Calling function {} at index {}", callee, idx);
                        chunk.emit(OpCode::Call(arguments.len() as u16, idx));
                    } else {
                        panic!("Function {} not found", callee);
                    }
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum ConstantType {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Function(FunctionObj),
}

#[derive(Debug, Clone)]
pub struct FunctionObj {
    pub name: String,
    pub arity: usize,
    pub chunk: Chunk,
}

#[derive(Debug, Clone, PartialEq)]
pub enum OpCode {
    LoadConst(usize),
    LoadLocal(usize),
    StoreLocal(usize),
    AddI64,
    AddF64,
    MulI64,
    NegI64,
    Call(u16, usize),
    Return,
    Halt,
    Print,
    MulF64,
}
