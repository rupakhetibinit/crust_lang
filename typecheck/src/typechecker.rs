use std::collections::HashMap;

use ast::TypeAnnotation;
use parser::ParserError;

use crate::{
    TypedLiteralValue,
    typed::{AstNode, AstNodeId, Type, TypedValueType},
};

pub struct TypeChecker {
    arena: ast::UntypedAstArena,
    typed_arena: AstNodeArena,
    env: HashMap<String, TypeAnnotation>,
}

impl TypeChecker {
    pub fn new(arena: ast::UntypedAstArena) -> Self {
        TypeChecker {
            arena,
            typed_arena: AstNodeArena::default(),
            env: HashMap::new(),
        }
    }

    pub fn type_check(&mut self, root_id: AstNodeId) -> Result<AstNodeId, ParserError> {
        let node = self.arena.get(root_id).clone();

        let typed_node = match node {
            ast::UntypedAstNode::BinaryExpression { left, op, right } => {
                let left_id = self.type_check(left)?;
                let right_id = self.type_check(right)?;
                let left_type = self.arena.get(left_id);
                let right_type = self.arena.get(right_id);
                let expr_type;

                match (left_type, right_type) {
                    (ast::UntypedAstNode::Literal(x), ast::UntypedAstNode::Literal(y)) => {
                        let left_type = match x {
                            LiteralValue::RawString(_) => Type::String,
                            LiteralValue::Int(_) => Type::I64,
                            LiteralValue::Float(_) => Type::F64,
                            LiteralValue::Bool(_) => Type::Bool,
                        };

                        let right_type = match x {
                            LiteralValue::RawString(_) => Type::String,
                            LiteralValue::Int(_) => Type::I64,
                            LiteralValue::Float(_) => Type::F64,
                            LiteralValue::Bool(_) => Type::Bool,
                        };

                        let left_node = self
                            .typed_arena
                            .alloc(AstNode::Literal(TypedLiteralValue::from(x)));
                        let right_node = self
                            .typed_arena
                            .alloc(AstNode::Literal(TypedLiteralValue::from(y)));

                        if left_type != right_type
                            || left_type == Type::Void
                            || right_type == Type::Void
                            || left_type == Type::String
                            || right_type == Type::String
                        {
                            return Err(ParserError::TypeError {
                                message: format!(
                                    "Type mismatch: {:?} and {:?} in binary expression",
                                    left_type, right_type
                                ),
                            });
                        } else {
                            expr_type = left_type;
                        }
                        self.typed_arena.alloc(AstNode::BinaryExpression(
                            left_node,
                            op.try_into().unwrap(),
                            right_node,
                            expr_type,
                        ))
                    }
                    _ => {
                        return Err(ParserError::TypeError {
                            message: format!(
                                "Invalid types for binary operation: {:?} and {:?}",
                                left_type, right_type
                            ),
                        });
                    }
                }
            }
            ast::UntypedAstNode::Literal(literal_value) => todo!(),
            ast::UntypedAstNode::Program(items) => {
                let mut statements = vec![];
                for statement in items {
                    let stmt = self.type_check(statement)?;
                    statements.push(stmt);
                }
                let root_id = self.typed_arena.alloc(AstNode::Program { statements });
                root_id
            }
            ast::UntypedAstNode::LetStatement {
                identifier,
                ty,
                value,
            } => {
                let typed_ast_expr_node_id = self.type_check(value)?;

                let typed_ast_node = self.typed_arena.get(typed_ast_expr_node_id).clone();

                let type_of_ast_node: Type = typed_ast_node.get_type().into();

                if let Some(ty) = ty {
                    let expected_type: Type = ty.ty.into();

                    if expected_type != type_of_ast_node {
                        return Err(ParserError::TypeError {
                            message: format!(
                                "Expected expression to be of type something but got something else"
                            ),
                        });
                    }
                };

                let let_ast_node = AstNode::LetStatement {
                    value: TypedValueType {
                        name: identifier.to_string(),
                        actual_type: type_of_ast_node.into(),
                    },
                    expr: typed_ast_expr_node_id,
                };

                let_ast_node
            }
            ast::UntypedAstNode::ReturnStatement { value } => todo!(),
            ast::UntypedAstNode::FunctionDefinition {
                name,
                parameters,
                return_type,
                body,
            } => todo!(),
            ast::UntypedAstNode::Block(items) => todo!(),
            ast::UntypedAstNode::Ident(_) => todo!(),
            ast::UntypedAstNode::Comment(_) => todo!(),
            ast::UntypedAstNode::ForLoop {
                initializer,
                condition,
                increment,
                block,
            } => todo!(),
            ast::UntypedAstNode::UnaryExpression { op, expression } => todo!(),
            ast::UntypedAstNode::FunctionCall { callee, arguments } => todo!(),
        };

        Ok(typed_node)
    }
}

#[derive(Default)]
pub struct AstNodeArena {
    nodes: Vec<AstNode>,
}

impl AstNodeArena {
    pub fn alloc(&mut self, node: AstNode) -> AstNodeId {
        let id = self.nodes.len();
        self.nodes.push(node);
        id
    }

    pub fn get(&self, id: AstNodeId) -> &AstNode {
        &self.nodes[id]
    }

    pub fn nodes(&self) -> &[AstNode] {
        &self.nodes
    }
}
