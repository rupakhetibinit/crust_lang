use crate::{error::report_parse_error, lexer::Lexer, parser::Parser};

mod bytecode;
mod error;
mod lexer;
mod parser;
mod typed;
mod untyped;
mod vm;

fn main() -> Result<(), ()> {
    let args = std::env::args();
    let args = args.skip(1);

    let source = "fn main(username: String, id: i64) -> void {
            let x = 2;
            let y == 2;
            return z;
            print(x);
           }

            fn test() -> void {
            let i = string;
            let y = i;
            println(1 + 2);
            }
            fn test(main: test, something: else)-> int{}
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
