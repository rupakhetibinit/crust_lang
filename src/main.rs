use logos::Logos;

use crate::lexer::Token;

mod lexer;
mod llvm;
mod parser;

fn main() {
    let option: RunOption = std::env::args()
        .nth(1)
        .expect("Options to be provided")
        .into();
    match option {
        RunOption::File => {
            let file_name = std::env::args().nth(1).expect("File name to be provided");
            let file_contents = std::fs::read_to_string(file_name).expect("File doesn't exist");

            run_file(&file_contents)
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
    let lexer = Token::lexer(file_contents);
    for token in lexer {
        println!("{:?}", token);
    }
}

fn run_repl() {
    let mut buf = String::new();

    std::io::stdin()
        .read_line(&mut buf)
        .expect("Failed to read line from stdin");

    let input = buf.trim_ascii();

    run_file(&input);
}
