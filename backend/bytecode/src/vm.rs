use std::fs;

use lexer::Lexer;
use parser::Parser;

pub struct CrustVM {}

impl CrustVM {
    pub fn new() -> Self {
        Self {}
    }

    pub fn run(&self, file_path: &str) -> () {
        let file_contents = fs::read_to_string(file_path).unwrap();

        let lexer = Lexer::new(&file_contents);

        let mut parser = Parser::new(&lexer.tokens);

        let (root_id, arena) = match parser.parse() {
            Ok(x) => x,
            Err(e) => {
                println!("{:?}", e);
                panic!("Something went seriously wrong");
            }
        };

        parser::print_ast(root_id, &arena);
    }
}
