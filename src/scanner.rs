use crate::{lox::error, token::*};

#[derive(Debug, Default)]
pub struct Scanner {
    source: String,
    tokens: Vec<Token>,
    line: usize,
    start: usize,
    current: usize,
}

impl Scanner {
    pub fn new(source: String) -> Self {
        Scanner {
            source,
            tokens: Vec::new(),
            line: 1,
            start: 0,
            current: 0,
        }
    }

    pub fn scan_tokens(mut self) -> Vec<Token> {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }

        self.tokens.push(Token {
            token_type: TokenType::Eof,
            lexeme: "".to_string(),
            literal: None,
            line: self.line,
        });

        self.tokens
    }

    pub fn is_at_end(&mut self) -> bool {
        self.current >= self.source.len()
    }

    pub fn scan_token(&mut self) {
        if let Some(c) = self.advance() {
            match c {
                '(' => self.add_token(TokenType::LeftParen),
                ')' => self.add_token(TokenType::RightParen),
                '{' => self.add_token(TokenType::LeftBrace),
                '}' => self.add_token(TokenType::RightBrace),
                ',' => self.add_token(TokenType::Comma),
                '.' => self.add_token(TokenType::Dot),
                '-' => self.add_token(TokenType::Minus),
                '+' => self.add_token(TokenType::Plus),
                ';' => self.add_token(TokenType::Semicolon),
                '*' => self.add_token(TokenType::Star),
                '!' => self.match_and_add_token(
                    '=',
                    (TokenType::EqualEqual, "=="),
                    (TokenType::BangEqual, "!="),
                ),
                '=' => self.match_and_add_token(
                    '=',
                    (TokenType::EqualEqual, "=="),
                    (TokenType::Equal, "!="),
                ),
                '<' => self.match_and_add_token(
                    '=',
                    (TokenType::LessEqual, "<="),
                    (TokenType::Less, "<"),
                ),
                '>' => self.match_and_add_token(
                    '=',
                    (TokenType::GreaterEqual, ">="),
                    (TokenType::Greater, ">"),
                ),
                '\n' => self.line += 1,
                _ => {
                    error(self.line, "Unexpected Character");
                }
            }
        }
    }

    pub fn match_and_add_token(
        &mut self,
        char_to_match: char,
        true_case: (TokenType, &str),
        false_case: (TokenType, &str),
    ) {
        let token_type = if self.match_token(char_to_match) {
            true_case
        } else {
            false_case
        };
        self.add_token_internal(token_type.0, Some(token_type.1.to_owned()));
    }

    pub fn match_token(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }
        if self
            .source
            .chars()
            .nth(self.current)
            .is_some_and(|x| x != expected)
        {
            return false;
        }
        self.current += 1;
        true
    }

    pub fn advance(&mut self) -> Option<char> {
        let result = self.source.chars().by_ref().nth(self.current);
        self.current += 1;
        result
    }

    fn add_token(&mut self, token_type: TokenType) {
        self.add_token_internal(token_type, None)
    }

    fn add_token_internal(&mut self, token_type: TokenType, literal: Option<String>) {
        if let Some(text) = self.source.chars().nth(self.current - 1) {
            self.tokens
                .push(Token::new(token_type, text.to_string(), literal, self.line))
        }
    }
}
