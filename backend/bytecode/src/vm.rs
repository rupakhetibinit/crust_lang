use std::fs;

use lexer::Lexer;
use parser::Parser;
use typecheck::{TypeChecker, TypedAstArena};

use crate::{codegen::CodeGen, internal::VirtualMachine};

pub struct CrustVM {}

impl CrustVM {
    pub fn new() -> Self {
        Self {}
    }

    pub fn run(&self, file_path: &str) -> Result<(), ()> {
        let file_contents = fs::read_to_string(file_path).unwrap();

        let lexer = Lexer::new(&file_contents);

        let mut parser = Parser::new(&lexer.tokens, &file_contents);

        let (mut root_id, arena) = match parser.parse() {
            Ok(x) => x,
            Err(error) => {
                eprintln!(
                    "{}",
                    error.display_with_source(&file_contents, Some(&file_path))
                );
                return Err(());
            }
        };

        let mut type_checker = TypeChecker::new(arena);
        let mut arena: TypedAstArena = TypedAstArena::default();

        match type_checker.type_check(root_id) {
            Ok((typed_root_id, typed_arena)) => {
                arena = typed_arena;
                root_id = typed_root_id;
            }
            Err(errors) => {
                println!("Type checking failed with {} errors:", errors.len());
                for error in errors {
                    println!("  {:?}", error);
                }
            }
        }

        let mut codegen = CodeGen::new(arena);
        let code = codegen.generate(root_id);
        // println!("{:?}", code);
        // code.disassemble();

        let mut virtual_machine = VirtualMachine::new();

        virtual_machine.run(&code);

        Ok(())
    }
}
