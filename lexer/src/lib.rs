#[cfg(test)]
mod tests;

use std::{iter::Peekable, ops::Range};

use logos::Logos;

#[derive(Logos, Debug, PartialEq)]
pub enum Token<'t> {
    #[token("->")]
    Arrow,

    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*")]
    Ident(&'t str),

    #[regex(r" +", |lex| lex.slice())]
    Space(&'t str),

    #[token("\t")]
    Tab,

    #[token("\n")]
    Newline,

    #[token("\r\n")]
    CarriageReturnNewLine,

    #[token("(")]
    LParen,

    #[token(")")]
    RParen,

    #[token("fn")]
    Fn,

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

    #[token("-")]
    Minus,

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
            Token::Space(spc) => format!("{}", spc),
            Token::Tab => format!("\t"),
            Token::Newline => format!("\n"),
            Token::CarriageReturnNewLine => format!("\r\n"),
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
        }
    }
}

pub struct SpannedToken<'t> {
    pub token: Token<'t>,
    pub span: Range<usize>,
}

pub struct Lexer<'l> {
    tokens: Peekable<std::vec::IntoIter<SpannedToken<'l>>>,
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

        Lexer {
            tokens: tokens.into_iter().peekable(),
        }
    }

    pub fn peek(&mut self) -> Option<&SpannedToken<'t>> {
        self.tokens.peek()
    }

    pub fn next(&mut self) -> Option<SpannedToken<'t>> {
        self.tokens.next()
    }
}
