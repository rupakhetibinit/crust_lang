use clap::{Parser, Subcommand};
use crust_backend::vm::CrustVM;

#[derive(Parser)]
struct Cli {
    #[clap(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    Run { file_path: String },
    Disassemble { file_path: String },
}

fn main() {
    let cli = Cli::parse();

    let bytecode_vm = CrustVM::new();

    match cli.command {
        Command::Run { file_path } => {
            _ = bytecode_vm.run_file(&file_path);
        }
        Command::Disassemble { file_path } => {
            _ = bytecode_vm.disassemble(&file_path);
        }
    }
}
