use std::{
    fs,
    io::{self, Write},
};

use crate::{
    ast::{AstNode, AstNodeId},
    interpreter::Interpreter,
    lexer::Lexer,
    parser::Parser,
    token::Token,
};

pub struct Crust {
    ast: Vec<AstNode>,
    roots: Vec<AstNodeId>,
}

impl Crust {
    pub fn new() -> Self {
        Crust {
            ast: Vec::new(),
            roots: Vec::new(),
        }
    }

    pub fn execute_snippet(&mut self, code: &str) {
        let mut lexer = Lexer::new(code);
        let mut tokens = Vec::new();
        loop {
            let tok = lexer.next_token();
            if tok == Token::EOF {
                break;
            }
            tokens.push(tok);
        }

        let mut snippet_ast = Vec::new();
        let mut snippet_roots = Vec::new();

        let mut parser = Parser::new(tokens, &mut snippet_ast, &mut snippet_roots);
        match parser.parse_all_statements() {
            Ok(_) => {}
            Err(e) => {
                eprintln!("Parse error: {:?}", e);
                return;
            }
        }

        let env = if !self.ast.is_empty() {
            let temp_interp = Interpreter::new(self.ast.clone(), self.roots.clone());
            temp_interp.get_environment()
        } else {
            Default::default()
        };

        let mut interp = Interpreter::new_with_env(snippet_ast.clone(), snippet_roots.clone(), env);
        if let Err(e) = interp.run() {
            eprintln!("Runtime error: {:?}", e);
        }

        self.ast = snippet_ast;
        self.roots = snippet_roots;

        let env = interp.get_environment();
        let updated_interp = Interpreter::new_with_env(self.ast.clone(), self.roots.clone(), env);
        self.ast = updated_interp.get_ast();
    }

    pub fn run_file(&mut self, path: &str) {
        let source = match fs::read_to_string(path) {
            Ok(s) => s,
            Err(e) => {
                eprintln!("Could not read {}: {}", path, e);
                return;
            }
        };
        self.execute_snippet(&source);
    }

    pub fn repl(&mut self) {
        println!("Welcome to Crust REPL! Type `exit` or `quit` to leave.");
        let stdin = io::stdin();

        let mut environment = Default::default();

        loop {
            print!("> ");
            let _ = io::stdout().flush();

            let mut line = String::new();
            if stdin.read_line(&mut line).is_err() {
                break;
            }
            let code = line.trim();
            if code.is_empty() {
                continue;
            }
            if code == "exit" || code == "quit" {
                break;
            }

            self.execute_repl_line(code, &mut environment);
        }
    }

    fn execute_repl_line(
        &mut self,
        code: &str,
        environment: &mut std::collections::HashMap<String, String>,
    ) {
        let mut lexer = Lexer::new(code);
        let mut tokens = Vec::new();
        loop {
            let tok = lexer.next_token();
            if tok == Token::EOF {
                break;
            }
            tokens.push(tok);
        }

        let mut line_ast = Vec::new();
        let mut line_roots = Vec::new();

        let mut parser = Parser::new(tokens, &mut line_ast, &mut line_roots);
        match parser.parse_all_statements() {
            Ok(_) => {}
            Err(e) => {
                eprintln!("Parse error: {:?}", e);
                return;
            }
        }

        let mut interp = Interpreter::new_with_env(line_ast, line_roots, environment.clone());
        if let Err(e) = interp.run() {
            eprintln!("Runtime error: {:?}", e);
        }

        *environment = interp.get_environment();
    }
}
