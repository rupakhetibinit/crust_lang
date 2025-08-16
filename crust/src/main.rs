use bytecode::vm::CrustVM;
use clap::{Parser, Subcommand};
use interpreter::crust::Crust;
use tracing_subscriber::{EnvFilter, FmtSubscriber};

#[derive(Parser)]
struct Cli {
    #[clap(subcommand)]
    command: Command,

    #[arg(short, long, value_enum, default_value_t = Backend::BytecodeVM)]
    backend: Backend,
}

#[derive(Subcommand)]
enum Command {
    Run { file_path: String },
    Disassemble { file_path: String },
    Repl {},
}

#[derive(Clone, Debug, clap::ValueEnum)]
enum Backend {
    Interpreter,
    BytecodeVM,
}

fn main() {
    let subscriber = FmtSubscriber::builder()
        .with_env_filter(
            EnvFilter::from_default_env().add_directive("your_crate=debug".parse().unwrap()),
        )
        .with_target(false) // Hide module paths for cleaner output
        .with_thread_ids(true) // Show thread IDs if needed
        .compact() // More compact output
        .finish();

    tracing::subscriber::set_global_default(subscriber).expect("Failed to set tracing subscriber");

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
            _ = bytecode_vm.run(&file_path);
        }
        (Backend::BytecodeVM, Command::Disassemble { file_path }) => {
            _ = bytecode_vm.disassemble(&file_path);
        }
        _ => panic!("Not supported"),
    }
}
