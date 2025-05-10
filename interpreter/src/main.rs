use std::env;

use new::crust::Crust;

fn main() {
    let mut crust = Crust::new();

    match env::args().nth(1).as_deref() {
        Some("repl") => {
            crust.repl();
        }
        Some(path) => {
            crust.run_file(path);
        }
        None => {
            eprintln!("Usage: crust <script.crust> | crust repl");
        }
    }
}
