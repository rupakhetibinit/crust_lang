use std::fs;

use lexer::Lexer;
use parser::Parser;

pub struct CrustVM {}

impl CrustVM {
    pub fn new() -> Self {
        Self {}
    }

    pub fn run(&self, file_path: &str) -> Result<(), ()> {
        let file_contents = fs::read_to_string(file_path).unwrap();

        let lexer = Lexer::new(&file_contents);

        let mut parser = Parser::new(&lexer.tokens, &file_contents);

        let (root_id, arena) = match parser.parse() {
            Ok(x) => x,
            Err(error) => {
                eprintln!(
                    "{}",
                    error.display_with_source(&file_contents, Some(&file_path))
                );
                return Err(());
            }
        };

        parser::print_ast(root_id, &arena);
        Ok(())
    }
}
