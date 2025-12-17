use logos::Logos;

#[derive(Logos, Debug, PartialEq)]
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
    Mul,
    #[token("/")]
    Div,
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
    #[token("|")]
    Pipe,
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
    #[token("||")]
    Or,
    #[token("&&")]
    And,
    #[regex("[0-9]+", |lex| lex.slice().parse::<isize>().unwrap(), priority = 3)]
    Integer(isize),
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("else if")]
    ElseIf,
}
