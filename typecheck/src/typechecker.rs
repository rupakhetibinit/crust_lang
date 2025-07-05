use std::collections::HashMap;

use ast::{BinOp, UnaryOp, UntypedAstArena, UntypedAstNode, UntypedAstNodeId};

use crate::{
    typed::{Type, TypedAstNode, TypedAstNodeId, TypedParameter},
    typed_arena::TypedAstArena,
};

#[derive(Debug, Clone)]
pub enum TypeError {
    UndefinedVariable(String),
    UndefinedFunction(String),
    TypeMismatch {
        expected: Type,
        found: Type,
        context: String,
    },
    InvalidBinaryOperation {
        left_type: Type,
        op: BinOp,
        right_type: Type,
    },
    InvalidUnaryOperation {
        op: UnaryOp,
        operand_type: Type,
    },
    ArityMismatch {
        function: String,
        expected: usize,
        found: usize,
    },
    NonBooleanCondition {
        found: Type,
    },
    InvalidReturnType {
        expected: Type,
        found: Type,
    },
}

#[derive(Debug, Clone)]
struct SymbolInfo {
    symbol_type: Type,
}

pub struct TypeChecker {
    untyped_arena: UntypedAstArena,
    typed_arena: TypedAstArena,
    scopes: Vec<HashMap<String, SymbolInfo>>,
    current_function_return_type: Option<Type>,
    errors: Vec<TypeError>,
}

impl TypeChecker {
    pub fn new(untyped_arena: UntypedAstArena) -> Self {
        let mut scopes = vec![HashMap::new()];

        scopes[0].insert(
            "print".to_string(),
            SymbolInfo {
                symbol_type: Type::Function {
                    params: vec![Type::Any],
                    return_type: Box::new(Type::Void),
                },
            },
        );

        Self {
            untyped_arena,
            typed_arena: TypedAstArena::default(),
            scopes,
            current_function_return_type: None,
            errors: Vec::new(),
        }
    }

    pub fn type_check(
        &mut self,
        root_id: UntypedAstNodeId,
    ) -> Result<(TypedAstNodeId, TypedAstArena), Vec<TypeError>> {
        match self.check_node(root_id) {
            Ok(typed_id) => {
                if self.errors.is_empty() {
                    Ok((typed_id, std::mem::take(&mut self.typed_arena)))
                } else {
                    Err(std::mem::take(&mut self.errors))
                }
            }
            Err(error) => {
                self.errors.push(error);
                Err(std::mem::take(&mut self.errors))
            }
        }
    }

    fn check_node(&mut self, node_id: UntypedAstNodeId) -> Result<TypedAstNodeId, TypeError> {
        let node = self.untyped_arena.get(node_id).clone();

        let typed_node = match node {
            UntypedAstNode::Program(statements) => {
                let mut typed_statements = Vec::new();
                for stmt_id in statements {
                    match self.check_node(stmt_id) {
                        Ok(typed_id) => typed_statements.push(typed_id),
                        Err(err) => self.errors.push(err),
                    }
                }
                TypedAstNode::Program {
                    statements: typed_statements,
                }
            }

            UntypedAstNode::Literal(value) => {
                let literal_type = Type::from(&value);
                TypedAstNode::Literal {
                    value,
                    literal_type,
                }
            }

            UntypedAstNode::Ident(name) => {
                let resolved_type = self
                    .lookup_symbol(&name)
                    .ok_or_else(|| TypeError::UndefinedVariable(name.clone()))?
                    .symbol_type;
                TypedAstNode::Identifier {
                    name,
                    resolved_type,
                }
            }

            UntypedAstNode::BinaryExpression { left, op, right } => {
                let typed_left = self.check_node(left)?;
                let typed_right = self.check_node(right)?;

                let left_type = self.typed_arena.get(typed_left).get_type().unwrap().clone();
                let right_type = self
                    .typed_arena
                    .get(typed_right)
                    .get_type()
                    .unwrap()
                    .clone();

                let result_type = self.check_binary_operation(&left_type, &op, &right_type)?;

                TypedAstNode::BinaryExpression {
                    left: typed_left,
                    op,
                    right: typed_right,
                    result_type,
                }
            }

            UntypedAstNode::UnaryExpression { op, expression } => {
                let typed_expr = self.check_node(expression)?;
                let expr_type = self.typed_arena.get(typed_expr).get_type().unwrap().clone();
                let result_type = self.check_unary_operation(&op, &expr_type)?;

                TypedAstNode::UnaryExpression {
                    op,
                    expression: typed_expr,
                    result_type,
                }
            }

            UntypedAstNode::LetStatement {
                identifier,
                ty,
                value,
            } => {
                let typed_value = self.check_node(value)?;
                let value_type = self
                    .typed_arena
                    .get(typed_value)
                    .get_type()
                    .unwrap()
                    .clone();

                let declared_type = if let Some(type_annotation) = ty {
                    let annotated_type = Type::from(&type_annotation);
                    if annotated_type != value_type {
                        return Err(TypeError::TypeMismatch {
                            expected: annotated_type,
                            found: value_type,
                            context: format!("variable declaration '{}'", identifier),
                        });
                    }
                    annotated_type
                } else {
                    value_type
                };

                // Add to current scope
                self.define_symbol(identifier.clone(), declared_type.clone());

                TypedAstNode::LetStatement {
                    identifier,
                    declared_type,
                    value: typed_value,
                }
            }

            UntypedAstNode::FunctionDefinition {
                name,
                parameters,
                return_type,
                body,
            } => {
                let typed_params: Vec<TypedParameter> =
                    parameters.iter().map(TypedParameter::from).collect();
                let func_return_type = Type::from(&return_type);

                // Create function type for symbol table
                let function_type = Type::Function {
                    params: typed_params.iter().map(|p| p.param_type.clone()).collect(),
                    return_type: Box::new(func_return_type.clone()),
                };

                // Add function to global scope
                self.scopes[0].insert(
                    name.clone(),
                    SymbolInfo {
                        symbol_type: function_type.clone(),
                    },
                );

                // Enter function scope
                self.push_scope();
                self.current_function_return_type = Some(func_return_type.clone());

                // Add parameters to function scope
                for param in &typed_params {
                    self.define_symbol(param.name.clone(), param.param_type.clone());
                }

                let typed_body = self.check_node(body)?;

                // Exit function scope
                self.pop_scope();
                self.current_function_return_type = None;

                TypedAstNode::FunctionDefinition {
                    name,
                    parameters: typed_params,
                    return_type: func_return_type,
                    body: typed_body,
                    function_type,
                }
            }

            UntypedAstNode::FunctionCall { callee, arguments } => {
                let symbol = self
                    .lookup_symbol(&callee)
                    .ok_or_else(|| TypeError::UndefinedFunction(callee.clone()))?;

                if let Type::Function {
                    params,
                    return_type,
                } = &symbol.symbol_type
                {
                    if arguments.len() != params.len() {
                        return Err(TypeError::ArityMismatch {
                            function: callee,
                            expected: params.len(),
                            found: arguments.len(),
                        });
                    }

                    let mut typed_args = Vec::new();
                    for (i, arg_id) in arguments.into_iter().enumerate() {
                        let typed_arg = self.check_node(arg_id)?;
                        let arg_type = self.typed_arena.get(typed_arg).get_type().unwrap();

                        if arg_type != &params[i] && params[i] != Type::Any {
                            return Err(TypeError::TypeMismatch {
                                expected: params[i].clone(),
                                found: arg_type.clone(),
                                context: format!("argument {} to function '{}'", i + 1, callee),
                            });
                        }
                        typed_args.push(typed_arg);
                    }

                    TypedAstNode::FunctionCall {
                        callee,
                        arguments: typed_args,
                        return_type: (**return_type).clone(),
                    }
                } else {
                    return Err(TypeError::UndefinedFunction(callee));
                }
            }

            UntypedAstNode::ReturnStatement { value } => {
                let typed_value = self.check_node(value)?;
                let value_type = self
                    .typed_arena
                    .get(typed_value)
                    .get_type()
                    .unwrap()
                    .clone();

                if let Some(expected_return_type) = &self.current_function_return_type {
                    if &value_type != expected_return_type {
                        return Err(TypeError::InvalidReturnType {
                            expected: expected_return_type.clone(),
                            found: value_type.clone(),
                        });
                    }
                }

                TypedAstNode::ReturnStatement {
                    value: typed_value,
                    return_type: value_type,
                }
            }

            UntypedAstNode::Block(statements) => {
                self.push_scope();
                let mut typed_statements = Vec::new();

                for stmt_id in statements {
                    match self.check_node(stmt_id) {
                        Ok(typed_id) => typed_statements.push(typed_id),
                        Err(err) => self.errors.push(err),
                    }
                }

                self.pop_scope();
                TypedAstNode::Block {
                    statements: typed_statements,
                }
            }

            UntypedAstNode::ForLoop {
                initializer,
                condition,
                increment,
                block,
            } => {
                self.push_scope();

                let typed_initializer = self.check_node(initializer)?;
                let typed_condition = self.check_node(condition)?;
                let typed_increment = self.check_node(increment)?;

                // Check that condition is boolean
                let condition_type = self
                    .typed_arena
                    .get(typed_condition)
                    .get_type()
                    .unwrap()
                    .clone();
                if condition_type != Type::Bool {
                    self.errors.push(TypeError::NonBooleanCondition {
                        found: condition_type.clone(),
                    });
                }

                let typed_block = self.check_node(block)?;

                self.pop_scope();

                TypedAstNode::ForLoop {
                    initializer: typed_initializer,
                    condition: typed_condition,
                    increment: typed_increment,
                    block: typed_block,
                }
            }

            UntypedAstNode::Comment(text) => TypedAstNode::Comment(text),
        };

        Ok(self.typed_arena.alloc(typed_node))
    }

    // === HELPER METHODS ===

    fn check_binary_operation(
        &self,
        left: &Type,
        op: &BinOp,
        right: &Type,
    ) -> Result<Type, TypeError> {
        match (left, op, right) {
            // Arithmetic operations
            (
                Type::I32,
                BinOp::Add | BinOp::Sub | BinOp::Multiply | BinOp::Divide | BinOp::Modulo,
                Type::I32,
            ) => Ok(Type::I32),
            (
                Type::I64,
                BinOp::Add | BinOp::Sub | BinOp::Multiply | BinOp::Divide | BinOp::Modulo,
                Type::I64,
            ) => Ok(Type::I64),
            (Type::F32, BinOp::Add | BinOp::Sub | BinOp::Multiply | BinOp::Divide, Type::F32) => {
                Ok(Type::F32)
            }
            (Type::F64, BinOp::Add | BinOp::Sub | BinOp::Multiply | BinOp::Divide, Type::F64) => {
                Ok(Type::F64)
            }

            // Comparison operations
            (
                Type::I32,
                BinOp::Equal | BinOp::NotEqual | BinOp::LesserEqual | BinOp::GreaterEqual,
                Type::I32,
            ) => Ok(Type::Bool),
            (
                Type::I64,
                BinOp::Equal | BinOp::NotEqual | BinOp::LesserEqual | BinOp::GreaterEqual,
                Type::I64,
            ) => Ok(Type::Bool),
            (
                Type::F32,
                BinOp::Equal | BinOp::NotEqual | BinOp::LesserEqual | BinOp::GreaterEqual,
                Type::F32,
            ) => Ok(Type::Bool),
            (
                Type::F64,
                BinOp::Equal | BinOp::NotEqual | BinOp::LesserEqual | BinOp::GreaterEqual,
                Type::F64,
            ) => Ok(Type::Bool),
            (Type::Bool, BinOp::Equal | BinOp::NotEqual, Type::Bool) => Ok(Type::Bool),

            // Logical operations
            (Type::Bool, BinOp::And | BinOp::Or, Type::Bool) => Ok(Type::Bool),

            _ => Err(TypeError::InvalidBinaryOperation {
                left_type: left.clone(),
                op: op.clone(),
                right_type: right.clone(),
            }),
        }
    }

    fn check_unary_operation(&self, op: &UnaryOp, operand: &Type) -> Result<Type, TypeError> {
        match (op, operand) {
            (UnaryOp::Negate, Type::I32) => Ok(Type::I32),
            (UnaryOp::Negate, Type::I64) => Ok(Type::I64),
            (UnaryOp::Negate, Type::F32) => Ok(Type::F32),
            (UnaryOp::Negate, Type::F64) => Ok(Type::F64),
            (UnaryOp::Not, Type::Bool) => Ok(Type::Bool),
            (
                UnaryOp::PreIncrement
                | UnaryOp::PreDecrement
                | UnaryOp::PostIncrement
                | UnaryOp::PostDecrement,
                Type::I32,
            ) => Ok(Type::I32),
            (
                UnaryOp::PreIncrement
                | UnaryOp::PreDecrement
                | UnaryOp::PostIncrement
                | UnaryOp::PostDecrement,
                Type::I64,
            ) => Ok(Type::I64),

            _ => Err(TypeError::InvalidUnaryOperation {
                op: op.clone(),
                operand_type: operand.clone(),
            }),
        }
    }

    // === SCOPE MANAGEMENT ===

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn define_symbol(&mut self, name: String, symbol_type: Type) {
        if let Some(current_scope) = self.scopes.last_mut() {
            current_scope.insert(name, SymbolInfo { symbol_type });
        }
    }

    fn lookup_symbol(&self, name: &str) -> Option<SymbolInfo> {
        for scope in self.scopes.iter().rev() {
            if let Some(symbol) = scope.get(name) {
                return Some(symbol.clone());
            }
        }
        None
    }
}
