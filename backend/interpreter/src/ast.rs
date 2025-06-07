#[derive(Debug, Clone)]
pub enum AstNode {
    Number(i64),
    Var(String),
    RawString(String),
    BinaryExpression(AstNodeId, BinOp, AstNodeId),
    Assign(String, AstNodeId),
    Reassignment(String, AstNodeId),
    Pow(AstNodeId, AstNodeId),
    Print(AstNodeId),
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
        else_block: Vec<AstNodeId>,
    },
    Return(AstNodeId),
    Equality(AstNodeId, AstNodeId),
}

#[derive(Debug, Clone)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Exp,
}

pub type AstNodeId = usize;
