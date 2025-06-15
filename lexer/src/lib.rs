#[cfg(test)]
mod tests;

mod lexer;

pub use lexer::{Lexer, SpannedToken, Token};
