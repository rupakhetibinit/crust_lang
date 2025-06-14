#[derive(Debug, Clone)]
pub enum AstNode<'a> {
    FunctionDecl {
        params: Vec<ValueType<'a>>,
        block: Vec<AstNodeId>,
        return_type: Type<'a>,
    },
    LetStatement {
        value: ValueType<'a>,
        expr: AstNodeId,
    },
    BinaryExpression(AstNodeId, BinOp, AstNodeId),
    UnaryExpression(UnaryOp, AstNodeId),
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
