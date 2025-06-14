use bytecode::vm::CrustVM;
use clap::{Parser, Subcommand};
use interpreter::crust::Crust;

#[derive(Parser)]
struct Cli {
    #[clap(subcommand)]
    command: Command,

    #[arg(short, long, value_enum, default_value_t = Backend::Interpreter)]
    backend: Backend,
}

#[derive(Subcommand)]
enum Command {
    Run { file_path: String },
    Repl {},
}

#[derive(Clone, Debug, clap::ValueEnum)]
enum Backend {
    Interpreter,
    BytecodeVM,
}

fn main() {
    let cli = Cli::parse();

    let mut crust_interpreter = Crust::new();
    let bytecode_vm = CrustVM::new();

    match (cli.backend, cli.command) {
        (Backend::Interpreter, Command::Run { file_path }) => {
            crust_interpreter.run_file(&file_path);
        }
        (Backend::Interpreter, Command::Repl {}) => {
            crust_interpreter.repl();
        }
        (Backend::BytecodeVM, Command::Run { file_path }) => {
            bytecode_vm.run(&file_path);
        }
        (Backend::BytecodeVM, Command::Repl {}) => todo!(),
    }
}
