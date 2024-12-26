use std::{
    fs,
    io::{self, Write},
};

use crate::{scanner::Scanner, HAD_ERROR};

pub struct Lox {}

impl Lox {
    pub fn new() -> Self {
        Lox {}
    }

    pub fn run_file(&mut self, path: &str) {
        let content = fs::read_to_string(path).expect("Couldn't read file");
        self.run(&content)
    }

    pub fn run(&mut self, content: &String) {
        let scanner = Scanner::new(content.to_owned());

        let tokens = scanner.scan_tokens();

        for token in tokens {
            println!("{}", token)
        }
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

            let mut had_error = HAD_ERROR.lock().unwrap();

            *had_error = false;
        }
    }
}

pub fn error(line: usize, message: &str) {
    report(line, "".to_string(), message);
}

pub fn report(line: usize, r#where: String, message: &str) {
    eprintln!("[line {} Error {} : {}", line, r#where, message);
}
