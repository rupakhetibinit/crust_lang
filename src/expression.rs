use crate::token::Token;

#[derive(Debug)]
pub enum Expr {
    Binary(BinaryExpr),
    Literal(LiteralExpr),
    Unary(UnaryExpr),
    Grouping(GroupingExpr),
    None,
}

#[derive(Debug)]
pub struct BinaryExpr {
    pub left: Box<Expr>,
    pub operator: Token,
    pub right: Box<Expr>,
}

#[derive(Debug)]
pub struct GroupingExpr {
    pub expr: Box<Expr>,
}

#[derive(Debug)]
pub struct LiteralExpr {
    pub literal: Option<String>,
}

#[derive(Debug)]
pub struct UnaryExpr {
    pub operator: Token,
    pub right: Box<Expr>,
}
