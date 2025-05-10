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
    ReAssign(String, AstNodeId),
    Pow(AstNodeId, AstNodeId),
    Div(AstNodeId, AstNodeId),
    Mul(AstNodeId, AstNodeId),
    Sub(AstNodeId, AstNodeId),
    Print(AstNodeId),
}

pub type AstNodeId = usize;
