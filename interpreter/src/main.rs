use std::{env, process};

// use old::lox::Lox;
use new::{lexer::Lexer, token::Token};

fn main() {
    let args: Vec<String> = env::args().collect();

    let mut lexer = Lexer::new("1 + 3 + 5 / 2");

    loop {
        let current = lexer.next_token();

        if current == Token::EOF {
            break;
        }
        println!("{current:?}");
    }

    // match args.len() {
    //     1 => lox.run_prompt(),
    //     2 => {
    //         let result = lox.run_file(&args[1]);
    //         println!("{}", result.unwrap())
    //     }
    //     _ => {
    //         println!("Usage: rust-lox [script]");
    //         process::exit(64);
    //     }
    // }
}
