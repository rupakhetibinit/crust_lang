use std::fmt::format;

use logos::Logos;

#[derive(Logos, Debug, PartialEq)]
pub enum Token<'t> {
    #[token("->")]
    Arrow,

    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*")]
    Ident(&'t str),

    #[token(" ")]
    Space,

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
            Token::Space => format!(" "),
            Token::Tab => format!("\t"),
            Token::Newline => format!("\n"),
            Token::CarriageReturnNewLine => format!("\r\n"),
        }
    }
}

#[cfg(test)]
mod tests {
    use logos::Logos;

    use super::Token;
    #[test]
    fn test_logos_lexer() {
        let source = "fn main() -> f64 { return 1 + 1.25; }";
        let output: String = Token::lexer(source)
            .map(|token| token.unwrap().to_string())
            .collect::<String>();

        let expected = "fn main() -> f64 { return 1 + 1.25; }";

        assert_eq!(expected, output);
    }

    #[test]
    fn test_number_lexing() {
        let source = "42 3.14 123.456";

        let output = Token::lexer(source)
            .map(|token| token.unwrap().to_string())
            .collect::<String>();

        assert_eq!(source, output);
    }
}
