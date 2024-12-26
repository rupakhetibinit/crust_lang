use std::sync::Mutex;
#[allow(dead_code)]
use std::{env, process};
mod lox;
mod scanner;
mod token;
use lox::Lox;
use once_cell::sync::Lazy;

pub static HAD_ERROR: Lazy<Mutex<bool>> = Lazy::new(|| Mutex::new(false));

fn main() {
    let args: Vec<String> = env::args().collect();
    let lox = Lox::new();
    match args.len() {
        1 => lox.run_prompt(),
        2 => lox.run_file(&args[1]),
        _ => {
            println!("Usage: rust-lox [script]");
            process::exit(64);
        }
    }
}
