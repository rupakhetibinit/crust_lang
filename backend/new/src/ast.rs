#[derive(Debug, Clone)]
pub struct AstNode {
    pub kind: AstKind,
}

#[derive(Debug, Clone)]
pub enum AstKind {
    Number(i64),
    Var(String),
    RawString(String),
    Add(AstNodeId, AstNodeId),
    Assign(String, AstNodeId),
    Reassignment(String, AstNodeId),
    Pow(AstNodeId, AstNodeId),
    Div(AstNodeId, AstNodeId),
    Mul(AstNodeId, AstNodeId),
    Sub(AstNodeId, AstNodeId),
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

pub type AstNodeId = usize;
