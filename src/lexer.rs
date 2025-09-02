use std::{fmt::format, ops::Range};

use logos::{Logos, Span};

#[derive(Logos, Debug, Clone)]
pub enum Token {
    #[regex(r"[ \t\n\r\f]+", logos::skip)]
    #[token("->")]
    Arrow,

    #[token("(")]
    LParen,

    #[token(")")]
    RParen,

    #[token("{")]
    LBrace,

    #[token("}")]
    RBrace,

    #[token("fn")]
    Fn,

    #[token("return")]
    Return,

    #[regex(r"[0-9]+\.[0-9]+", |lex| lex.slice().parse::<f64>().unwrap())]
    Float(f64),

    #[regex(r"[0-9]+", |lex| lex.slice().parse::<i64>().unwrap())]
    Int(i64),

    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_string())]
    Ident(String),

    #[token(";")]
    SemiColon,

    #[token(".")]
    Dot,

    #[token("-")]
    Minus,

    #[token("+")]
    Plus,

    #[token("/")]
    Slash,

    #[token("*")]
    Star,

    #[token("%")]
    Modulo,

    #[token("::")]
    DoubleColon,

    #[token(":")]
    Colon,

    #[token("!")]
    Not,

    #[token(">=")]
    GreaterEqual,

    #[token("<=")]
    LesserEqual,

    #[token("==")]
    EqualEqual,

    #[token("!=")]
    NotEqual,

    #[token(">")]
    RightAngleBracket,

    #[token("<")]
    LeftAngleBracket,

    #[token("=")]
    Equal,

    #[token("?")]
    QuestionMark,

    #[token("&&")]
    And,

    #[token("||")]
    Or,

    #[token("&")]
    BitAnd,

    #[token("|")]
    BitOr,

    #[token("let")]
    Let,

    #[token("const")]
    Const,

    #[token("impl")]
    Impl,

    #[token("for")]
    For,

    #[token("++")]
    PlusPlus,

    #[token("--")]
    MinusMinus,

    #[token(",")]
    Comma,

    #[regex(r#""[^"]*""#, parse_string_slice)]
    StringLiteral(String),

    #[regex(r"//[^\n]*", |lex| lex.slice().to_string())]
    LineComment(String),

    #[regex(r"/\*[^*]*\*+(?:[^/*][^*]*\*+)*/", |lex| lex.slice().to_string())]
    MultiLineComment(String),
}

fn parse_string_slice<'lex>(lex: &mut logos::Lexer<Token>) -> Option<String> {
    let slice = lex.slice();
    Some(slice[1..slice.len() - 1].to_string())
}

impl<'t> ToString for Token {
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
            Token::SemiColon => format!(";"),
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
            Token::MultiLineComment(comment) => format!("{}", comment),
            Token::Impl => format!("impl"),
        }
    }
}

pub struct SpannedToken {
    pub token: Token,
    pub span: Range<usize>,
}

pub struct Lexer {
    pub tokens: Vec<SpannedToken>,
    pub errors: Vec<String>,
}

impl Lexer {
    pub fn new(source: String) -> Lexer {
        let mut lex = Token::lexer(&source);

        let mut tokens = Vec::new();

        let mut errors = Vec::<String>::new();

        while let Some(result) = lex.next() {
            let span = lex.span();
            match result {
                Ok(token) => tokens.push(SpannedToken { token, span }),
                Err(_) => errors.push(format!(
                    "Lexing error at {:?} '{}'",
                    lex.span(),
                    &source[span.clone()]
                )),
            }
        }

        Lexer { tokens, errors }
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::{Lexer, Token};

    #[test]
    pub fn lex_example_programs() {
        let sources = [
            "
for(let i = 0; i <=5 ; i++) {
    let x = i * i; 
}


for(let i = 0; i <=5 ; ++i) {
    let x  = i * i;
    let x = 2.0;
    let y : f64 = x * 0.5;
}
        ",
            "
fn main(x: i32, y: i32) -> i32 {
    let x = \"String example\";
    return x * 2 * y;
}

// This is a comment
fn test(x : i32, y : f32, z : String) -> f32 {
    return x + y
}
        ",
            "
fn test() -> i64 {
    return 2;
}

/* This is a test for multi line comment with the 
virtual machine and the bytecode compiler */

fn main() -> void {
    let z = 2 * 22;
    print(z);
}
        ",
        ];
        for source in sources {
            let lexer = Lexer::new(source.to_string());

            assert!(lexer.errors.is_empty());
        }
    }
}
