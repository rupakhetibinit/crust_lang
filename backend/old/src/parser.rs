use codespan::{FileId, Files};
use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    term::{
        self,
        termcolor::{ColorChoice, StandardStream},
    },
};

use crate::{
    error::Error,
    expression::{Expr, Object},
    syntax::Stmt,
    token::{Token, TokenType},
};

macro_rules! matches {
    ( $sel:ident, $( $x:expr ),* ) => {
        {
            if $( $sel.check($x) )||* {
                $sel.advance();
                true
            } else {
                false
            }
        }
    };
}

#[allow(dead_code)]
#[derive(Debug)]
pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
    files: Files<String>,
    file_id: FileId,
    source: String,
}

impl Parser {
    pub fn new(source: String, tokens: Vec<Token>) -> Self {
        let mut files = Files::new();
        let file_id = files.add("source", source.clone());

        Parser {
            tokens,
            current: 0,
            files,
            file_id,
            source,
        }
    }
    pub fn parse(&mut self) -> Result<Vec<Stmt>, Error> {
        let mut statements = Vec::<Stmt>::with_capacity(20);
        while !self.is_at_end() {
            statements.push(self.statement()?);
        }
        Ok(statements)
    }

    fn statement(&mut self) -> Result<Stmt, Error> {
        if matches!(self, TokenType::Print) {
            return self.print_statement();
        }
        self.expression_statement()
    }

    fn equality(&mut self) -> Result<Expr, Error> {
        let mut expr = self.comparison()?;

        while matches!(self, TokenType::BangEqual, TokenType::EqualEqual) {
            let operator = self.previous().to_owned();
            let right = self.comparison()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            }
        }

        Ok(expr)
    }

    fn advance(&mut self) -> Token {
        if !self.is_at_end() {
            self.current += 1
        }
        self.previous()
    }

    fn check(&self, token_type: TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }
        self.peek().token_type == token_type
    }

    fn is_at_end(&self) -> bool {
        self.peek().token_type == TokenType::Eof
    }

    fn peek(&self) -> &Token {
        self.tokens
            .get(self.current)
            .expect("Peek into the end of token stream")
    }

    fn previous(&self) -> Token {
        self.tokens
            .get(self.current - 1)
            .expect("Previous was empty")
            .clone()
    }

    fn comparison(&mut self) -> Result<Expr, Error> {
        let mut expr = self.term()?;
        while matches!(
            self,
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual
        ) {
            let operator = self.previous();
            let right = self.term()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr, Error> {
        let mut expr = self.factor()?;

        while matches!(self, TokenType::Minus, TokenType::Plus) {
            let operator = self.previous();
            let right = self.factor()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            }
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr, Error> {
        let mut expr = self.unary()?;

        while matches!(self, TokenType::Slash, TokenType::Star) {
            let operator = self.previous();
            let right = self.unary()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, Error> {
        if matches!(self, TokenType::Bang, TokenType::Minus) {
            let operator = self.previous();
            let right = self.unary()?;
            return Ok(Expr::Unary {
                operator,
                right: Box::new(right),
            });
        }
        self.primary()
    }

    fn primary(&mut self) -> Result<Expr, Error> {
        let expr = match self.peek().clone().token_type {
            TokenType::False => Expr::Literal {
                value: Object::Boolean(false),
            },
            TokenType::True => Expr::Literal {
                value: Object::Boolean(true),
            },
            TokenType::Nil => Expr::Literal {
                value: Object::Null,
            },
            TokenType::String { literal } => Expr::Literal {
                value: Object::String(literal),
            },
            TokenType::Number { literal } => Expr::Literal {
                value: Object::Number(literal),
            },
            TokenType::LeftParen => {
                self.advance();
                let expr = self.expression()?;

                self.consume(TokenType::RightParen, "Expected ')' after expression.")?;

                Expr::Grouping {
                    expr: Box::new(expr),
                }
            }
            _ => return Err(self.error(self.peek(), "Expected expression")),
        };

        self.advance();

        Ok(expr)
    }

    fn consume(&mut self, token_type: TokenType, message: &str) -> Result<Token, Error> {
        if self.check(token_type) {
            Ok(self.advance())
        } else {
            let token = self.peek().clone();
            Err(self.error(&token, message))
        }
    }

    fn expression(&mut self) -> Result<Expr, Error> {
        self.equality()
    }

    fn error(&self, token: &Token, message: &str) -> Error {
        let writer = StandardStream::stderr(ColorChoice::Always);
        let config = term::Config::default();

        let diagnostic = Diagnostic::error().with_message(message).with_labels(vec![
            Label::primary(self.file_id, token.span.0..token.span.1).with_message(message),
        ]);

        term::emit(&mut writer.lock(), &config, &self.files, &diagnostic)
            .expect("Failed to emit diagnostic");

        Error::Parse
    }

    #[allow(unused)]
    fn synchronize(&mut self) {
        self.advance();

        while !self.is_at_end() {
            if self.previous().token_type == TokenType::Semicolon {
                return;
            }

            match self.peek().token_type {
                TokenType::Class
                | TokenType::Fun
                | TokenType::Var
                | TokenType::For
                | TokenType::If
                | TokenType::Return
                | TokenType::While => return,
                _ => {}
            }

            self.advance();
        }
    }

    pub fn files(&self) -> Files<String> {
        self.files.clone()
    }

    pub fn file_id(&mut self) -> FileId {
        self.file_id.clone()
    }

    fn print_statement(&mut self) -> Result<Stmt, Error> {
        let value = self.expression()?;
        self.consume(TokenType::Semicolon, "Expect ; SEMICOLON after Expression")?;
        Ok(Stmt::Print { expression: value })
    }

    fn expression_statement(&mut self) -> Result<Stmt, Error> {
        let value = self.expression()?;
        self.consume(TokenType::Semicolon, "Expect ; SEMICOLON after Value")?;
        Ok(Stmt::Expression { expression: value })
    }
}

// #[cfg(test)]
// mod tests {
//     use super::*;
//     use crate::expression::AstPrinter;
//     use crate::scanner::Scanner;

//     #[test]
//     fn test_parser() {
//         let scanner = Scanner::new("-123.45 * 56.78;".to_string());

//         let (source, tokens) = scanner.scan_tokens();

//         let mut parser = Parser::new(source, tokens);

//         let expression = parser.parse().expect("Could not parse sample code");

//         let printer = AstPrinter;
//         let expr = expression.get(0).unwrap();
//         expr.

//         assert_eq!(
//             printer.print(&expression[0].expression).unwrap(),
//             "(* (- 123.45) 56.78)"
//         )
//     }
// }
