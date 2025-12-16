use crate::{error::report_parse_error, lexer::Lexer, parser::Parser};

mod error;
mod lexer;
mod parser;

fn main() -> Result<(), ()> {
    let source = "fn main() -> int {
            let x = 2;
            let y = 2;
            return x + y;
            }
           "
    .to_owned();

    let lexer = Lexer::new(source.clone());

    assert!(lexer.errors.is_empty());

    let mut parser = Parser::new(lexer.tokens, "main.rs".into());

    let program = parser.parse_program();

    match program {
        Ok(p) => {
            println!("{:?}", p)
        }
        Err(e) => report_parse_error(&source, &e),
    }

    Ok(())
}
