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

    let thing = source;

    let output = Token::lexer(thing);

    for lex in output {
        println!("{:?}", lex)
    }

    let output = Token::lexer(source)
        .map(|token| token.unwrap().to_string())
        .collect::<String>();

    assert_eq!(source, output);
}
