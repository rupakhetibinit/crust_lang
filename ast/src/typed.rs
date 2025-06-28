use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum AstNode {
    Program {
        statements: Vec<AstNodeId>,
    },
    FunctionDecl {
        params: Vec<ValueType>,
        block: AstNodeId,
        return_type: Type,
    },
    Block {
        statements: Vec<AstNodeId>,
    },
    LetStatement {
        value: ValueType,
        expr: AstNodeId,
    },
    BinaryExpression(AstNodeId, BinOp, AstNodeId),
    UnaryExpression(UnaryOp, AstNodeId),
    StructDecl {
        name: String,
        methods: Vec<AstNodeId>,
        fields: HashMap<String, ValueType>,
    },
    StructMethodDecl {
        params: Vec<ValueType>,
        block: Vec<AstNodeId>,
        return_type: Type,
    },
    FunctionCall {
        args: Vec<AstNodeId>,
    },
    StructMethodCall {},
}

pub type AstNodeId = usize;

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct ValueType {
    name: String,
    annotated_type: Type,
}

#[derive(Debug, Clone)]
pub enum Type {
    TypeValue(String),
    InferrableType,
}

#[derive(Debug, Clone)]
pub enum BinOp {
    Add,
    Sub,
    Multiply,
    Divide,
    Modulo,
    Or,
    And,
    NotEqual,
    Equal,
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Plus,
    Minus,
    Not,
}
