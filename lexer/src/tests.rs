use logos::Logos;

use crate::Token;

#[test]
fn test_logos_lexer() {
    let source = "fn main() -> f64 { return 1 + 1.25; }";

    let tokens: Vec<Token> = Token::lexer(source).map(|token| token.unwrap()).collect();

    assert_eq!(
        tokens,
        vec![
            Token::Fn,
            Token::Ident("main"),
            Token::LParen,
            Token::RParen,
            Token::Arrow,
            Token::Ident("f64"),
            Token::LBrace,
            Token::Return,
            Token::Int(1),
            Token::Plus,
            Token::Float(1.25),
            Token::Semicolon,
            Token::RBrace,
        ]
    );
}

#[test]
fn test_number_lexing() {
    let source = "42 3.14 123.456";

    let tokens: Vec<Token> = Token::lexer(source).map(|token| token.unwrap()).collect();

    assert_eq!(
        tokens,
        vec![Token::Int(42), Token::Float(3.14), Token::Float(123.456),]
    );
}

#[test]
fn test_full_language() {
    let source = r#"
            fn main(thing : Whatever) -> Result<String> {
            // this is a comment
            // this is also a comment
                let x: u32 = 2;
                const y: i32 = 3;
                let stringliteral2123asdf = "string is here";
                x: u64 = x + 5;
                let thing: isize = if (x == 2) { x } else { y };
                let y = 2;

                if (y != 2) {
                    print("this is amazing");
                }

                let z: f64 = 0.02;
                print(x,y,z);
            }
        "#;

    let tokens: Vec<Token> = Token::lexer(source).map(|token| token.unwrap()).collect();

    assert!(tokens.contains(&Token::Fn));
    assert!(tokens.contains(&Token::Arrow));
    assert!(tokens.contains(&Token::Let));
    assert!(tokens.contains(&Token::Const));

    let token_pairs: Vec<(&Token, &Token)> = tokens.iter().zip(tokens.iter().skip(1)).collect();

    for (t1, t2) in token_pairs.iter() {
        if let Token::Let = t1 {
            if let Token::Ident(_) = t2 {
            } else {
                panic!("'let' not followed by identifier");
            }
        }
    }
}

#[test]
fn test_comments() {
    let source = "// This is a comment\nlet x = 5;";

    let tokens: Vec<Token> = Token::lexer(source).map(|token| token.unwrap()).collect();

    assert_eq!(
        tokens,
        vec![
            Token::LineComment("// This is a comment".into()),
            Token::Let,
            Token::Ident("x"),
            Token::Equal,
            Token::Int(5),
            Token::Semicolon,
        ]
    );
}

#[test]
fn test_string_literals() {
    let source = r#"let msg = "Hello, world!";"#;

    let tokens: Vec<Token> = Token::lexer(source).map(|token| token.unwrap()).collect();

    assert_eq!(
        tokens,
        vec![
            Token::Let,
            Token::Ident("msg"),
            Token::Equal,
            Token::StringLiteral("Hello, world!"),
            Token::Semicolon,
        ]
    );
}
