use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum AstNode<'arena> {
    Program {
        statements: Vec<AstNodeId>,
    },
    FunctionDecl {
        params: Vec<ValueType<'arena>>,
        block: AstNodeId,
        return_type: Type<'arena>,
    },
    Block {
        statements: Vec<AstNodeId>,
    },
    LetStatement {
        value: ValueType<'arena>,
        expr: AstNodeId,
    },
    BinaryExpression(AstNodeId, BinOp, AstNodeId),
    UnaryExpression(UnaryOp, AstNodeId),
    StructDecl {
        name: &'arena str,
        methods: Vec<AstNodeId>,
        fields: HashMap<&'arena str, ValueType<'arena>>,
    },
    StructMethodDecl {
        params: Vec<ValueType<'arena>>,
        block: Vec<AstNodeId>,
        return_type: Type<'arena>,
    },
    FunctionCall {
        args: Vec<AstNodeId>,
    },
    StructMethodCall {},
}

pub type AstNodeId = usize;

#[derive(Debug, Clone)]
pub struct ValueType<'t> {
    name: &'t str,
    annotated_type: Type<'t>,
}

#[derive(Debug, Clone)]
pub enum Type<'t> {
    TypeValue(&'t str),
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
