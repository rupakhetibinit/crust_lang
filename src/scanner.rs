use codespan::{FileId, Files};
use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    term::{
        self,
        termcolor::{ColorChoice, StandardStream},
    },
};

use crate::{token::*, KEYWORDS};

#[derive(Debug)]
pub struct Scanner {
    chars: Vec<(usize, char)>, // (byte_offset, char)
    current: usize,
    start: usize,
    tokens: Vec<Token>,
    line: usize,
    files: Files<String>,
    file_id: FileId,
    source: String,
}

impl Scanner {
    pub fn new(source: String) -> Self {
        let mut files = Files::new();
        let file_id = files.add("source", source.clone());

        let chars: Vec<(usize, char)> = source.char_indices().collect();
        Scanner {
            chars,
            current: 0,
            start: 0,
            tokens: Vec::new(),
            line: 1,
            files,
            file_id,
            source,
        }
    }

    pub fn scan_tokens(mut self) -> (String, Vec<Token>) {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }

        let eof_span = if !self.source.is_empty() {
            (self.source.len(), self.source.len())
        } else {
            (0, 0)
        };

        self.tokens.push(Token {
            token_type: TokenType::Eof,
            lexeme: "".to_string(),
            literal: None,
            line: self.line,
            span: eof_span,
        });

        (self.source, self.tokens)
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.chars.len()
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
                        while (self.peek().is_some() && self.peek().unwrap() != '\n')
                            && !self.is_at_end()
                        {
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
                        self.error("Unexpected character");
                    }
                }
            }
        }
    }

    fn match_identifier(&mut self) {
        // Process alphanumeric characters
        while let Some(c) = self.peek() {
            if c.is_alphanumeric() {
                self.advance();
            } else {
                break;
            }
        }

        // Calculate byte spans using original char indices
        let start_byte = self.chars[self.start].0;
        let end_byte = if self.current < self.chars.len() {
            self.chars[self.current].0
        } else {
            self.source.len()
        };

        // Extract identifier text using byte offsets
        let text = self.source[start_byte..end_byte].to_string();

        // Determine token type
        let token_type = KEYWORDS
            .get(&text[..])
            .cloned()
            .unwrap_or(TokenType::Identifier);

        // Add token with proper span information
        self.add_token_internal(token_type, Some(text));
    }

    fn match_number(&mut self) {
        // Process integer part
        while let Some(c) = self.peek() {
            if c.is_ascii_digit() {
                self.advance();
            } else {
                break;
            }
        }

        // Look for decimal point with fractional part
        if self.peek() == Some('.') {
            // Check if next character is a digit
            if self.peek_next().map_or(false, |c| c.is_ascii_digit()) {
                self.advance(); // Consume the '.'

                // Process fractional part
                while let Some(c) = self.peek() {
                    if c.is_ascii_digit() {
                        self.advance();
                    } else {
                        break;
                    }
                }
                true
            } else {
                // Report error if decimal is not followed by digit
                self.error("Invalid decimal number");
                false
            }
        } else {
            false
        };

        // Calculate span using original char indices
        let start_byte = self.chars[self.start].0;
        let end_byte = self.chars[self.current - 1].0 + 1; // +1 to include last char

        // Extract numeric literal
        let number_str = &self.source[start_byte..end_byte];
        let literal = number_str.parse::<f64>().unwrap_or_else(|_| {
            self.error("Invalid numeric literal");
            0.0 // Default value, error already reported
        });

        self.add_token_internal(TokenType::Number { literal }, Some(literal.to_string()));
    }

    fn match_string(&mut self) {
        let start_line = self.line;
        let string_start = self.chars[self.start].0;

        while let Some((_, c)) = self.chars.get(self.current) {
            match c {
                '"' => break,
                '\n' => self.line += 1,
                _ => {}
            }
            self.current += 1;
        }

        if self.is_at_end() {
            self.report_error(
                "Unterminated string",
                string_start,
                self.source.len(),
                start_line,
            );
            return;
        }

        // Consume the closing quote
        self.current += 1;

        let start_byte = string_start + 1; // Skip opening quote
        let end_byte = self.chars[self.current - 1].0; // Before closing quote
        let value = self.source[start_byte..end_byte].to_string();

        self.add_token_internal(
            TokenType::String {
                literal: value.clone(),
            },
            Some(value),
        );
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

    fn add_token(&mut self, token_type: TokenType) {
        self.add_token_internal(token_type, None)
    }

    fn add_token_internal(&mut self, token_type: TokenType, literal: Option<String>) {
        let start_byte = self.chars[self.start].0;
        let end_byte = if self.current < self.chars.len() {
            self.chars[self.current].0
        } else {
            self.source.len()
        };

        let lexeme = self.source[start_byte..end_byte].to_string();

        self.tokens.push(Token::new(
            token_type,
            lexeme,
            literal,
            self.line,
            (self.start, self.current),
        ));
    }

    fn report_error(&self, message: &str, start: usize, end: usize, line: usize) {
        let writer = StandardStream::stderr(ColorChoice::Always);
        let config = term::Config::default();

        let diagnostic = Diagnostic::error()
            .with_message(message)
            .with_labels(vec![Label::primary(self.file_id, start..end)
                .with_message(format!("[line {}] {}", line, message))]);

        term::emit(&mut writer.lock(), &config, &self.files, &diagnostic)
            .expect("Failed to emit diagnostic");
    }

    // Updated error handling
    fn error(&mut self, message: &str) {
        let start = self.chars[self.start].0;
        let end = if self.current < self.chars.len() {
            self.chars[self.current].0
        } else {
            self.source.len()
        };
        self.report_error(message, start, end, self.line);
    }

    // Updated advance and peek methods
    fn advance(&mut self) -> Option<char> {
        if self.current < self.chars.len() {
            let c = self.chars[self.current].1;
            self.current += 1;
            Some(c)
        } else {
            None
        }
    }

    fn peek(&self) -> Option<char> {
        self.chars.get(self.current).map(|(_, c)| *c)
    }

    fn peek_next(&self) -> Option<char> {
        self.chars.get(self.current + 1).map(|(_, c)| *c)
    }
}
