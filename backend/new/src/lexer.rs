use std::error::Error;
use std::{iter::Peekable, path::Path, str::Chars};

use crate::token::Token;

pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            input: src.chars().peekable(),
        }
    }

    pub fn new_from_file(file_path: &Path) -> Result<Lexer<'a>, Box<dyn Error>> {
        let file_contents = std::fs::read_to_string(file_path)?;

        Ok(Lexer {
            input: Box::leak(file_contents.into_boxed_str()).chars().peekable(),
        })
    }

    fn consume_while<F: Fn(char) -> bool>(&mut self, cond: F) -> String {
        let mut s = String::new();
        while let Some(&ch) = self.input.peek() {
            if cond(ch) {
                s.push(ch);
                self.input.next();
            } else {
                break;
            }
        }
        s
    }

    pub fn next_token(&mut self) -> Token {
        while let Some(&ch) = self.input.peek() {
            match ch {
                ' ' | '\t' | '\r' | '\n' => {
                    self.input.next();
                }
                '=' => {
                    self.input.next();
                    return Token::Equal;
                }
                '+' => {
                    self.input.next();
                    return Token::Plus;
                }
                '-' => {
                    self.input.next();
                    return Token::Minus;
                }
                '*' => {
                    self.input.next();
                    return Token::Star;
                }
                '/' => {
                    self.input.next();
                    return Token::Slash;
                }
                '0'..'9' => {
                    let number = self.consume_while(|c| c.is_numeric());
                    return Token::Number(number.parse::<i64>().unwrap());
                }
                'a'..='z' | 'A'..='Z' | '_' => {
                    let ident = self.consume_while(|c| c.is_alphabetic() || c == '_');

                    return match ident.as_str() {
                        "let" => Token::Let,
                        _ => Token::Ident(ident),
                    };
                }
                '"' => {
                    self.input.next();

                    let ident = self.consume_while(|c| c.is_alphabetic());
                    let expect = '"';

                    if let Some(&x) = self.input.peek()
                        && x != expect
                    {
                        panic!("Invalid string termination")
                    }

                    self.input.next();

                    return Token::RawString(ident);
                }
                ';' => {
                    self.input.next();
                    return Token::Semicolon;
                }
                '(' => {
                    self.input.next();
                    return Token::LParen;
                }
                ')' => {
                    self.input.next();
                    return Token::RParen;
                }
                _ => return Token::EOF,
            }
        }

        Token::EOF
    }
}
