use std::ops::Range;

use logos::Logos;

#[derive(Logos, Debug, PartialEq, Clone)]
pub enum Token<'t> {
    #[regex(r"[ \t\n\r\f]+", logos::skip)]
    #[token("->")]
    Arrow,

    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*")]
    Ident(&'t str),

    #[token("(")]
    LParen,

    #[token(")")]
    RParen,

    #[token("fn")]
    Fn,

    #[token("for")]
    For,

    #[regex(r"[0-9]+\.[0-9]+", |lex| lex.slice().parse::<f64>().unwrap())]
    Float(f64),

    #[regex(r"[0-9]+", |lex| lex.slice().parse::<i64>().unwrap())]
    Int(i64),

    #[token("{")]
    LBrace,

    #[token("}")]
    RBrace,

    #[token(";")]
    Semicolon,

    #[token("return")]
    Return,

    #[token("+")]
    Plus,

    #[token("++")]
    PlusPlus,

    #[token("-")]
    Minus,

    #[token("--")]
    MinusMinus,

    #[token("*")]
    Star,

    #[token("/")]
    Slash,

    #[regex(r"//[^\n]*", |lex| lex.slice().to_string())]
    LineComment(String),

    #[token("%")]
    Modulo,

    #[regex(r#""[^"]*""#, parse_string_slice)]
    StringLiteral(&'t str),

    #[token("let")]
    Let,

    #[token("const")]
    Const,

    #[token(",")]
    Comma,

    #[token("<")]
    LeftAngleBracket,

    #[token(">")]
    RightAngleBracket,

    #[token("=")]
    Equal,

    #[token("==")]
    EqualEqual,

    #[token(">=")]
    GreaterEqual,

    #[token("<=")]
    LesserEqual,

    #[token("&")]
    BitAnd,

    #[token("|")]
    BitOr,

    #[token("||")]
    Or,

    #[token("&&")]
    And,

    #[token(":")]
    Colon,

    #[token("?")]
    QuestionMark,

    #[token("::")]
    DoubleColon,

    #[token("!")]
    Not,

    #[token("!=")]
    NotEqual,

    #[token(".")]
    Dot,
}

fn parse_string_slice<'lex>(lex: &mut logos::Lexer<'lex, Token<'lex>>) -> Option<&'lex str> {
    let slice = lex.slice();
    Some(&slice[1..slice.len() - 1])
}

impl<'t> ToString for Token<'t> {
    fn to_string(&self) -> String {
        match self {
            Token::Arrow => format!("->"),
            Token::Ident(ident) => format!("{}", ident),
            Token::LParen => format!("("),
            Token::RParen => format!(")"),
            Token::Fn => format!("fn"),
            Token::Float(f) => format!("{}", f),
            Token::Int(i) => format!("{}", i),
            Token::LBrace => format!("{{"),
            Token::RBrace => format!("}}"),
            Token::Semicolon => format!(";"),
            Token::Return => format!("return"),
            Token::Plus => format!("+"),
            Token::Minus => format!("-"),
            Token::Star => format!("*"),
            Token::Slash => format!("/"),
            Token::LineComment(comment) => format!("{}", comment),
            Token::Modulo => format!("%"),
            Token::StringLiteral(s) => format!("\"{}\"", s),
            Token::Let => format!("let"),
            Token::Const => format!("const"),
            Token::Comma => format!(","),
            Token::LeftAngleBracket => format!("<"),
            Token::RightAngleBracket => format!(">"),
            Token::Equal => format!("="),
            Token::EqualEqual => format!("=="),
            Token::GreaterEqual => format!(">="),
            Token::LesserEqual => format!("<="),
            Token::BitAnd => format!("&"),
            Token::BitOr => format!("|"),
            Token::Or => format!("||"),
            Token::And => format!("&&"),
            Token::Colon => format!(":"),
            Token::QuestionMark => format!("?"),
            Token::DoubleColon => format!("::"),
            Token::Not => format!("!"),
            Token::NotEqual => format!("!="),
            Token::Dot => format!("."),
            Token::For => format!("for"),
            Token::PlusPlus => format!("++"),
            Token::MinusMinus => format!("--"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct SpannedToken<'t> {
    pub token: Token<'t>,
    pub span: Range<usize>,
}

pub struct Lexer<'l> {
    pub tokens: Vec<SpannedToken<'l>>,
}

impl<'t> Lexer<'t> {
    pub fn new(source: &'t str) -> Lexer<'t> {
        let mut lex = Token::lexer(source);

        let mut tokens = Vec::new();
        let mut errors = Vec::<String>::new();

        while let Some(result) = lex.next() {
            let span = lex.span();

            match result {
                Ok(token) => {
                    tokens.push(SpannedToken { token, span });
                }
                Err(_) => {
                    errors.push(format!(
                        "Lexing error at {:?} '{}' ",
                        span,
                        &source[span.clone()]
                    ));
                }
            }
        }

        Lexer { tokens: tokens }
    }
}
