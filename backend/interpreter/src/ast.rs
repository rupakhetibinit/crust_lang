#[derive(Debug, Clone)]
pub enum AstNode {
    Number(i64),
    Var(String),
    RawString(String),
    BinaryExpression(AstNodeId, BinOp, AstNodeId),
    PostIncrement(String),
    Assign(String, AstNodeId),
    Reassignment(String, AstNodeId),
    Pow(AstNodeId, AstNodeId),
    Print(Vec<AstNodeId>),
    FunctionDeclaration {
        name: String,
        args: Vec<String>,
        block: Vec<AstNodeId>,
    },
    FunctionCall {
        func: AstNodeId,
        args: Vec<AstNodeId>,
    },
    If {
        expression: AstNodeId,
        block: Vec<AstNodeId>,
        else_if_blocks: Option<Vec<(AstNodeId, Vec<AstNodeId>)>>,
        else_block: Option<Vec<AstNodeId>>,
    },
    Return(AstNodeId),
    Equality(AstNodeId, AstNodeId),
    Comparison(AstNodeId, ComparisonOp, AstNodeId),
    ForLoop {
        init: AstNodeId,
        condition: AstNodeId,
        increment: AstNodeId,
        body: Vec<AstNodeId>,
    },
}

#[derive(Debug, Clone)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Exp,
}

#[derive(Debug, Clone)]
pub enum ComparisonOp {
    LessOrEqual,
    Less,
    GreaterOrEqual,
    Greater,
}

pub type AstNodeId = usize;
