use crate::{lox::error, token::*, KEYWORDS};

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

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn scan_token(&mut self) {
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
                    (TokenType::BangEqual, "!="),
                    (TokenType::Bang, "!"),
                ),
                '=' => self.match_and_add_token(
                    '=',
                    (TokenType::EqualEqual, "=="),
                    (TokenType::Equal, "="),
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
                '/' => {
                    if self.match_token('/') {
                        while (self.peek() != '\n') && !self.is_at_end() {
                            self.advance();
                        }
                    } else {
                        self.add_token(TokenType::Slash);
                    }
                }
                '\n' => self.line += 1,
                '\r' | '\t' | ' ' => {}
                '"' => self.match_string(),
                _ => {
                    if c.is_ascii_digit() {
                        self.match_number();
                    } else if c.is_alphabetic() {
                        self.match_identifier();
                    } else {
                        error(self.line, "Unexpected Character");
                    }
                }
            }
        }
    }

    fn match_identifier(&mut self) {
        while self.peek().is_alphanumeric() {
            self.advance();
        }

        let text = self
            .source
            .get(self.start..self.current)
            .unwrap()
            .to_string();

        let token_type = if let Some(token_type) = KEYWORDS.get(text.as_str()) {
            token_type.clone()
        } else {
            TokenType::Identifier
        };

        self.add_token(token_type);
    }

    fn match_number(&mut self) {
        while self.peek().is_ascii_digit() {
            self.advance();
        }

        if self.peek() == '.' && self.peek_next().is_ascii_digit() {
            self.advance();
            while self.peek().is_ascii_digit() {
                self.advance();
            }
        }

        self.add_token_internal(
            TokenType::Number,
            Some(
                self.source
                    .get(self.start..self.current)
                    .unwrap()
                    .to_string(),
            ),
        );
    }

    fn peek_next(&self) -> char {
        if self.current + 1 > self.source.len() {
            return '\0';
        }
        self.source.chars().nth(self.current + 1).unwrap()
    }

    fn match_string(&mut self) {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            error(self.line, "Unterminated string");
        }

        self.advance();

        let value: Option<String> = Some(
            self.source
                .get(self.start + 1..self.current - 1)
                .unwrap()
                .to_string(),
        );

        self.add_token_internal(TokenType::String, value);
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            return '\0';
        }
        self.source.chars().nth(self.current).unwrap()
    }

    fn match_and_add_token(
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

    fn match_token(&mut self, expected: char) -> bool {
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

    fn advance(&mut self) -> Option<char> {
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
