use std::{
    fs,
    io::{self, Write},
};

use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    term::{
        self,
        termcolor::{ColorChoice, StandardStream},
    },
};

use crate::{error::Error, interpreter::Interpreter, parser::Parser, scanner::Scanner, HAD_ERROR};

pub struct Lox;

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

        let statements = parser.parse();

        let files = parser.files().clone();
        let file_id = parser.file_id().clone();

        let interpreter = Interpreter;

        match statements {
            Ok(stmts) => {
                for stmt in stmts {
                    match interpreter.evaluate(&stmt) {
                        Ok(result) => {}
                        Err(e) => match e {
                            Error::Runtime { token, message } => {
                                let writer = StandardStream::stderr(ColorChoice::Always);
                                let config = codespan_reporting::term::Config::default();

                                let diagnostic = Diagnostic::error()
                                    .with_message(message.clone())
                                    .with_labels(vec![Label::primary(
                                        file_id,
                                        token.span.0..token.span.1,
                                    )
                                    .with_message(message)]);

                                term::emit(&mut writer.lock(), &config, &files, &diagnostic)
                                    .unwrap();
                            }
                            _ => {}
                        },
                    }
                }
            }
            Err(e) => match e {
                Error::Runtime { token, message } => {
                    let writer = StandardStream::stderr(ColorChoice::Always);
                    let config = codespan_reporting::term::Config::default();

                    let diagnostic = Diagnostic::error()
                        .with_message(message.clone())
                        .with_labels(vec![Label::primary(file_id, token.span.0..token.span.1)
                            .with_message(message)]);

                    term::emit(&mut writer.lock(), &config, &files, &diagnostic).unwrap();
                }
                Error::Parse => {}
            },
        }
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
