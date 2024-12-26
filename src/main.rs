#[allow(dead_code)]
use std::{env, process};
mod lox;
mod scanner;
mod token;
use lox::Lox;

fn main() {
    let args: Vec<String> = env::args().collect();
    let mut lox = Lox::new();
    match args.len() {
        1 => lox.run_prompt(),
        2 => lox.run_file(&args[1]),
        _ => {
            println!("Usage: rust-lox [script]");
            process::exit(64);
        }
    }
}
