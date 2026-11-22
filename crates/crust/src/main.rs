use clap::{Parser, Subcommand};
use crust_backend::Engine;

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
    let engine = Engine::new();

    let result = match cli.command {
        Command::Run { file_path } => engine.run_file(&file_path).map(|_| ()),
        Command::Disassemble { file_path } => engine.disassemble_file(&file_path).map(|ir| {
            print!("{ir}");
        }),
    };

    if let Err(error) = result {
        eprintln!("{error}");
        std::process::exit(1);
    }
}
