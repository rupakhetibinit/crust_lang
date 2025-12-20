use inkwell::context::Context;

use crate::parser::Parser;

mod lexer;
mod llvm;
mod parser;

fn main() {
    let option: RunOption = std::env::args()
        .nth(1)
        .expect("Options to be provided")
        .into();

    let cwd = std::env::current_dir().expect("Failed to get current dir");

    match option {
        RunOption::File => {
            let file_name = std::env::args().nth(2).expect("File name to be provided");
            let file_name = cwd.join(file_name);
            let file_contents = std::fs::read_to_string(file_name).expect("File doesn't exist");
            run_file(file_contents.trim_ascii())
        }
        RunOption::Repl => run_repl(),
    }
}

enum RunOption {
    File,
    Repl,
}

impl From<String> for RunOption {
    fn from(s: String) -> Self {
        match s.as_str() {
            "--file" | "-f" => RunOption::File,
            "--repl" | "-r" => RunOption::Repl,
            _ => panic!("Invalid option"),
        }
    }
}

fn run_file(file_contents: &str) {
    let mut parser = Parser::new(file_contents);
    let program = parser.parse_program();
    println!("Parsed program: {:?}", program);
    let context = Context::create();
    let mut compiler = llvm::CodeGen::new(&context, "my_program");
    compiler.compile_program(program);
    compiler.save_ir();
}

fn run_repl() {
    let mut buf = String::new();

    std::io::stdin()
        .read_line(&mut buf)
        .expect("Failed to read line from stdin");

    let input = buf.trim_ascii();

    run_file(input);
}
