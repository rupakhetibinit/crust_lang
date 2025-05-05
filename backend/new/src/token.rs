#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Let,
    Ident(String),
    Number(i64),
    Plus,
    Minus,
    Multiply,
    Divide,
    Equal,
    Semicolon,
    LParen,
    RParen,
    EOF,
}
