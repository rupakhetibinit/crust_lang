use logos::Logos;

use crate::lexer::Token;

mod bytecode;
mod lexer;
mod parser;
mod typed;
mod untyped;
mod vm;
fn main() -> Result<(), ()> {
    let mut lexer = Token::lexer("fn main() { return 0; }");

    while let Some(lex) = lexer.next() {
        match lex {
            Ok(token) => {
                println!("Token parsed : {:?}", token)
            }
            Err(e) => {
                eprintln!("Error {:?}", e)
            }
        }
    }

    Ok(())
}
