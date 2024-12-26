use std::{
    fs,
    io::{self, Write},
};

use crate::scanner::Scanner;

pub struct Lox {
    has_error: bool,
}

impl Lox {
    pub fn new() -> Self {
        Lox { has_error: false }
    }

    pub fn run_file(&mut self, path: &str) {
        let content = fs::read_to_string(path).expect("Couldn't read file");
        self.run(&content)
    }

    pub fn run(&mut self, content: &String) {
        let scanner = Scanner::new(content.to_owned());

        let tokens = scanner.scan_tokens();

        for token in tokens {
            println!("{:#?}", token)
        }
    }

    pub fn error(&mut self, line: i32, message: &str) {
        self.report(line, "".to_string(), message);
    }

    pub fn report(&mut self, line: i32, r#where: String, message: &str) {
        eprintln!("[line {} Error {} : {}", line, r#where, message);
    }

    pub fn run_prompt(&mut self) {
        let mut input = String::new();
        loop {
            print!("> ");
            io::stdout().flush().unwrap();

            io::stdin()
                .read_line(&mut input)
                .expect("Fatal Error. Failed to read input");

            if input.trim().is_empty() {
                break;
            }

            println!("{:?}", input.trim());
            self.run(&input);
            input.clear();
            self.has_error = false
        }
    }
}
