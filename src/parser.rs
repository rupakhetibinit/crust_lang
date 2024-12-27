use crate::{
    expression::{BinaryExpr, Expr, GroupingExpr, LiteralExpr, UnaryExpr},
    lox::report,
    token::{Token, TokenType},
};
#[derive(Debug)]
pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }
    pub fn parse(&mut self) {}

    fn equality(&mut self) -> Expr {
        let mut expr = self.comparison();

        while self.match_token(vec![TokenType::BangEqual, TokenType::EqualEqual]) {
            let operator = self.previous().to_owned();
            let right = self.comparison();
            expr = Expr::Binary(BinaryExpr {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            })
        }

        expr
    }

    fn match_token(&mut self, types: Vec<TokenType>) -> bool {
        for token in types {
            if self.check(token) {
                self.advance();
                return true;
            }
        }
        false
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
        self.tokens.get(self.current).unwrap()
    }

    fn previous(&self) -> Token {
        self.tokens.as_slice()[self.current - 1].clone()
    }

    fn comparison(&mut self) -> Expr {
        let mut expr = self.term();
        while self.match_token(vec![
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ]) {
            let operator = self.previous();
            let right = self.term();
            expr = Expr::Binary(BinaryExpr {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            })
        }
        expr
    }

    fn term(&mut self) -> Expr {
        let mut expr = self.factor();
        while self.match_token(vec![TokenType::Minus, TokenType::Plus]) {
            let operator = self.previous();
            let right = self.factor();
            expr = Expr::Binary(BinaryExpr {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            })
        }
        expr
    }

    fn factor(&mut self) -> Expr {
        let mut expr = self.unary();

        while self.match_token(vec![TokenType::Slash, TokenType::Star]) {
            let operator = self.previous();
            let right = self.unary();
            expr = Expr::Binary(BinaryExpr {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            });
        }
        expr
    }

    fn unary(&mut self) -> Expr {
        if self.match_token(vec![TokenType::Bang, TokenType::Minus]) {
            let operator = self.previous();
            let right = self.unary();
            return Expr::Unary(UnaryExpr {
                operator,
                right: Box::new(right),
            });
        }
        self.primary()
    }

    fn primary(&mut self) -> Expr {
        if self.match_token(vec![TokenType::False]) {
            return Expr::Literal(LiteralExpr {
                literal: Some(false.to_string()),
            });
        }
        if self.match_token(vec![TokenType::True]) {
            return Expr::Literal(LiteralExpr {
                literal: Some(true.to_string()),
            });
        }
        if self.match_token(vec![TokenType::Nil]) {
            return Expr::Literal(LiteralExpr {
                literal: Some("null".to_string()),
            });
        }

        if self.match_token(vec![TokenType::Number, TokenType::String]) {
            return Expr::Literal(LiteralExpr {
                literal: self.previous().literal,
            });
        }

        if self.match_token(vec![TokenType::LeftParen]) {
            let expr = self.expression();
            self.consume(TokenType::RightParen, "Expect ) after expression");
            Expr::Grouping(GroupingExpr {
                expr: Box::new(expr),
            })
        } else {
            Expr::None
        }
    }

    fn consume(&mut self, token_type: TokenType, message: &str) -> Token {
        if self.check(token_type) {
            return self.advance();
        }
        self.error(self.peek().clone(), message);
        Token::new(TokenType::Eof, "".to_string(), None, 1)
    }

    fn error(&self, token: Token, message: &str) {
        if token.token_type == TokenType::Eof {
            report(token.line, " at end".to_string(), message);
        } else {
            report(token.line, format!("at '{}'", token.lexeme), message);
        }
    }

    fn expression(&mut self) -> Expr {
        self.equality()
    }
}
