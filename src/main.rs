use crate::{
    lexer::Lexer,
    parser::{Parser, report_parse_error},
};

mod bytecode;
mod lexer;
mod parser;
mod typed;
mod untyped;
mod vm;
fn main() -> Result<(), ()> {
    let args = std::env::args();
    let args = args.skip(1);

    let source =
        "fn main(username: String, id: i64 ) -> i64  { let x = 2; let z = x + 3; return z; }"
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
