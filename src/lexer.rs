use logos::Logos;

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(skip r"[ \t\n\r\f]+")]
pub enum Token {
    #[token("fn")]
    Fn,
    #[token("let")]
    Let,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
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
    #[regex("[a-zA-Z0-9]+", |lex| lex.slice().to_string())]
    Identifier(String),

    #[token("=>")]
    Arrow,
    #[token(":")]
    Colon,
    #[token(";")]
    Semicolon,
    #[token(",")]
    Comma,
    #[token("->")]
    TypeArrow,
    #[token("|")]
    Pipe,
    #[token("||")]
    Or,
    #[token("&&")]
    And,
    #[regex("[0-9]+", |lex| lex.slice().parse::<i64>().unwrap(), priority = 3)]
    Integer(i64),
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("else if")]
    ElseIf,
}
