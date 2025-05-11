use std::{
    collections::HashMap,
    fs,
    io::{self, Write},
};

use crate::{
    ast::{AstNode, AstNodeId},
    interpreter::{EvalOutcome, Function, Interpreter},
    lexer::Lexer,
    parser::Parser,
    token::Token,
};

pub struct Crust {
    ast: Vec<AstNode>,
    roots: Vec<AstNodeId>,
    functions: HashMap<String, Function>,
    env: HashMap<String, EvalOutcome>,
}

impl Crust {
    pub fn new() -> Self {
        Self {
            ast: Vec::new(),
            roots: Vec::new(),
            functions: HashMap::new(),
            env: HashMap::new(),
        }
    }

    fn execute_line(&mut self, code: &str) {
        let old_roots_len = self.roots.len();

        let mut lex = Lexer::new(code);
        let mut tokens = Vec::new();
        loop {
            let tok = lex.next_token();
            if tok == Token::EOF {
                break;
            }
            tokens.push(tok);
        }

        let mut parser = Parser::new(tokens, &mut self.ast, &mut self.roots);
        if let Err(e) = parser.parse_all_statements() {
            eprintln!("Parse error: {:?}", e);
            return;
        }

        let new_roots = self.roots[old_roots_len..].to_vec();

        let mut interp = Interpreter::new_with_env(
            self.ast.clone(),
            new_roots,
            self.env.clone(),
            self.functions.clone(),
        );
        if let Err(e) = interp.run() {
            eprintln!("Runtime error: {:?}", e);
        }

        self.env = interp.get_environment();
        self.functions = interp.into_functions();
    }

    pub fn repl(&mut self) {
        println!("Welcome to Crust REPL! Type `exit` or `quit` to leave.");
        let stdin = io::stdin();

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
            self.execute_line(code);
        }
    }

    pub fn run_file(&mut self, path: &str) {
        let src = match fs::read_to_string(path) {
            Ok(s) => s,
            Err(e) => {
                eprintln!("Could not read {}: {}", path, e);
                return;
            }
        };

        self.execute_line(&src);
    }
}
