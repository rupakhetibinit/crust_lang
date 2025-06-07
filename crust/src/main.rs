use clap::{Parser, Subcommand};
use interpreter::crust::Crust;

#[derive(Parser)]
struct Cli {
    #[clap(subcommand)]
    command: Command,

    #[arg(short, long, default_value = "interpreter")]
    backend: Option<String>,
}

#[derive(Subcommand)]
enum Command {
    Run { file_path: String },
    Repl {},
}

fn main() {
    let cli = Cli::parse();

    let mut crust = Crust::new();

    if let Some(backend) = cli.backend {
        if backend != "interpreter" {
            eprintln!("Unsupported backend: {}", backend);
            return;
        }
    }

    match cli.command {
        Command::Run { file_path } => {
            crust.run_file(&file_path);
        }
        Command::Repl {} => {
            crust.repl();
        }
    }
}
