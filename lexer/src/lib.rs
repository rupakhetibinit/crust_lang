use logos::Logos;

#[derive(Logos, Debug, PartialEq)]
pub enum Token<'t> {
    #[token("->")]
    Arrow,

    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*")]
    Ident(&'t str),

    #[regex(r"[ \t\n\f]+")]
    Whitespace,

    #[token("(")]
    LParen,

    #[token(")")]
    RParen,

    #[token("fn")]
    Fn,

    #[regex(r"[0-9]+\.[0-9]+", priority = 4)]
    Float(&'t str),

    #[regex(r"[0-9]+", priority = 3)]
    Int(&'t str),

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
}

#[cfg(test)]
mod tests {
    use logos::Logos;

    use super::Token;
    #[test]
    fn test_logos_lexer() {
        let source = "fn main() -> f64 { return 1 + 1.25; }";
        let lex = Token::lexer(source);

        let lexed = [
            Token::Fn,
            Token::Whitespace,
            Token::Ident("main"),
            Token::LParen,
            Token::RParen,
            Token::Whitespace,
            Token::Arrow,
            Token::Whitespace,
            Token::Ident("f64"),
            Token::Whitespace,
            Token::LBrace,
            Token::Whitespace,
            Token::Return,
            Token::Whitespace,
            Token::Int("1"),
            Token::Whitespace,
            Token::Plus,
            Token::Whitespace,
            Token::Float("1.25"),
            Token::Semicolon,
            Token::Whitespace,
            Token::RBrace,
        ];
        for (actual, expected) in lex.zip(lexed).into_iter() {
            assert_eq!(actual.unwrap(), expected);
        }
    }

    #[test]
    fn test_number_lexing() {
        let source = "42 3.14 123.456";
        let lex = Token::lexer(source);

        let lexed = [
            Token::Int("42"),
            Token::Whitespace,
            Token::Float("3.14"),
            Token::Whitespace,
            Token::Float("123.456"),
        ];

        for (actual, expected) in lex.zip(lexed).into_iter() {
            assert_eq!(actual.unwrap(), expected);
        }
    }
}
