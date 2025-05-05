use std::{env, process};

use old::lox::Lox;

fn main() {
    let args: Vec<String> = env::args().collect();
    let lox = Lox::new();
    match args.len() {
        1 => lox.run_prompt(),
        2 => {
            let result = lox.run_file(&args[1]);
            println!("{}", result.unwrap())
        }
        _ => {
            println!("Usage: rust-lox [script]");
            process::exit(64);
        }
    }
}
