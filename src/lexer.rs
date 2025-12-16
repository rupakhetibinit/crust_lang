use logos::Logos;

#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f]+")]
pub enum Token {
    #[token("fn")]
    Fn,
    #[token("let")]
    Let,
    #[token("+")]
    Plus,
    #[token("i32")]
    I32,
    #[token("u32")]
    U32,
    #[token("i64")]
    I64,
    #[token("u64")]
    U64,
    #[token(">")]
    GreaterThan,
    #[token(">=")]
    GreaterThanOrEqual,
    #[token("==")]
    EqualEqual,
    #[token("<")]
    LessThan,
    #[token("<=")]
    LessThanOrEqual,
    #[token("!=")]
    NotEqual,
    #[token("=")]
    Equal,
    #[token(")")]
    RightParen,
    #[token("(")]
    LeftParen,
    #[token("]")]
    RightSqBracket,
    #[token("[")]
    LeftSqBracket,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("return")]
    Return,
    #[regex("[a-zA-Z]+")]
    Text,
}
