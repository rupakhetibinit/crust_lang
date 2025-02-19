use crate::{
    error::{parser_error, Error},
    expression::{Expr, Object},
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

#[derive(Debug)]
pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }
    pub fn parse(&mut self) -> Option<Expr> {
        self.expression().ok()
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
            return Ok(self.advance());
        } else {
            Err(self.error(self.peek(), message))
        }
    }

    fn expression(&mut self) -> Result<Expr, Error> {
        self.equality()
    }

    fn error(&self, token: &Token, message: &str) -> Error {
        parser_error(token.clone(), message);
        Error::Parse
    }

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
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::expression::AstPrinter;
    use crate::scanner::Scanner;

    #[test]
    fn test_parser() {
        let scanner = Scanner::new("-123.45 * 56.78".to_string());

        let tokens = scanner.scan_tokens();

        let mut parser = Parser::new(tokens);

        let expression = parser.parse().expect("Could not parse sample code");

        let printer = AstPrinter;

        assert_eq!(printer.print(expression).unwrap(), "(* (- 123.45) 56.78)")
    }
}
