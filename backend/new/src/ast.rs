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
    Add(AstNodeId, AstNodeId),
    Assign(Symbol, AstNodeId),
}

#[derive(Debug)]
struct Value {
    data: i64,
}

new_key_type! {
    pub struct AstNodeId;
    pub struct ValueId;
}

pub type Symbol = DefaultSymbol;
