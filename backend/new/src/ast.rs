use slotmap::new_key_type;
use string_interner::DefaultSymbol;

#[derive(Debug)]
pub struct AstNode {
    pub kind: AstKind,
}

#[derive(Debug)]
pub enum AstKind {
    Number(i64),
    Var(Symbol),
    RawString(Symbol),
    Add(AstNodeId, AstNodeId),
    Assign(Symbol, AstNodeId),
    Pow(AstNodeId, AstNodeId),
    Div(AstNodeId, AstNodeId),
    Mul(AstNodeId, AstNodeId),
    Sub(AstNodeId, AstNodeId),
    Print(AstNodeId),
}

new_key_type! {
    pub struct AstNodeId;
    pub struct ValueId;
}

pub type Symbol = DefaultSymbol;
