#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    Let,
    Ident(String),
    RawString(String),
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
