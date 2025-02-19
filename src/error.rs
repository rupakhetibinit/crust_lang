use std::fmt::Display;

use crate::{
    lox::report,
    token::{Token, TokenType},
};

#[derive(Debug)]
pub enum Error {
    Parse,
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Parse => write!(f, "Parse Error"),
        }
    }
}

pub fn parser_error(token: Token, message: &str) {
    if token.token_type == TokenType::Eof {
        report(token.line, " at end".to_string(), message);
    } else {
        report(token.line, "".to_string(), message);
    }
}
