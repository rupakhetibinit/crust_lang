use std::{
    fs,
    io::{self, Write},
};

use crate::{
    expression::AstPrinter, interpreter::Interpreter, parser::Parser, scanner::Scanner, HAD_ERROR,
};

pub struct Lox {}

impl Lox {
    pub fn new() -> Self {
        Lox {}
    }

    pub fn run_file(&self, path: &str) {
        let content = fs::read_to_string(path).expect("Couldn't read file");
        self.run(&content)
    }

    pub fn run(&self, content: &String) {
        let scanner = Scanner::new(content.to_owned());

        let (source, tokens) = scanner.scan_tokens();

        let mut parser = Parser::new(source, tokens);

        let had_error = HAD_ERROR.lock().unwrap();

        if *had_error == true {
            return;
        };

        let expression = parser.parse();

        let printer = AstPrinter;
        let interpreter = Interpreter;

        if expression.is_none() {
            return;
        }

        println!("{}", printer.print(expression.as_ref().unwrap()).unwrap());

        println!("{:?}", interpreter.evaluate(&expression.unwrap()));
    }

    pub fn run_prompt(&self) {
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
    println!("[line {} Error {} : {}", line, r#where, message);
}
