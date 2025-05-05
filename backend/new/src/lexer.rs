use core::num;
use std::{iter::Peekable, str::Chars};

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
                    return Token::Multiply;
                }
                '/' => {
                    self.input.next();
                    return Token::Divide;
                }
                '0'..'9' => {
                    let number = self.consume_while(|c| c.is_numeric());
                    return Token::Number(number.parse::<i64>().unwrap());
                }

                _ => return Token::EOF,
            }
        }

        Token::EOF
    }
}
