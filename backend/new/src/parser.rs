use crate::{
    ast::{AstKind, AstNode, AstNodeId},
    token::Token,
};

use std::iter::Peekable;

pub struct Parser<'a> {
    tokens: Peekable<std::vec::IntoIter<Token>>,
    pub ast: &'a mut Vec<AstNode>,
    pub roots: &'a mut Vec<AstNodeId>,
}

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken(String),
    Unreachable,
}

impl<'a> Parser<'a> {
    pub fn new(
        tokens: Vec<Token>,
        ast: &'a mut Vec<AstNode>,
        roots: &'a mut Vec<AstNodeId>,
    ) -> Self {
        Self {
            tokens: tokens.into_iter().peekable(),
            ast,
            roots,
        }
    }

    pub fn peek(&mut self) -> &Token {
        self.tokens.peek().unwrap_or(&Token::EOF)
    }

    fn next(&mut self) -> Token {
        self.tokens.next().unwrap_or(Token::EOF)
    }

    fn expect(&mut self, expect: Token) -> Result<(), ParseError> {
        let next = self.next();

        if next != expect {
            Err(ParseError::UnexpectedToken(format!(
                "Expected {:?}, got {:?}",
                expect, next
            )))
        } else {
            Ok(())
        }
    }

    pub fn parse_all_statements(&mut self) -> Result<(), ParseError> {
        while self.peek() != &Token::EOF {
            let stmt_id = self.parse_stmt()?;
            self.roots.push(stmt_id);
        }
        Ok(())
    }

    pub fn parse_stmt(&mut self) -> Result<AstNodeId, ParseError> {
        let node_id = match self.peek() {
            Token::Let => self.parse_let()?,
            Token::Print => self.parse_print()?,
            Token::Ident(_) => {
                if let Some(Token::Equal) = self.tokens.clone().into_iter().nth(1) {
                    self.parse_ident()?
                } else {
                    let expr = self.parse_expr(0)?;
                    self.expect(Token::Semicolon)?;
                    expr
                }
            }
            _ => {
                let expr = self.parse_expr(0)?;
                self.expect(Token::Semicolon)?;
                expr
            }
        };

        Ok(node_id)
    }

    fn parse_expr(&mut self, min_prec: u8) -> Result<AstNodeId, ParseError> {
        let mut left = self.parse_primary()?;

        while let Some(prec) = precedence(self.peek()) {
            if prec < min_prec {
                break;
            }

            let op = self.next();

            let right = self.parse_expr(prec + 1)?;

            let kind = match op {
                Token::Plus => AstKind::Add(left, right),
                Token::Minus => AstKind::Sub(left, right),
                Token::Star => AstKind::Mul(left, right),
                Token::Slash => AstKind::Div(left, right),
                Token::Caret => AstKind::Pow(left, right),
                _ => return Err(ParseError::Unreachable),
            };

            let id = self.ast.len();

            self.ast.push(AstNode { kind });

            left = id;
        }

        Ok(left)
    }

    fn parse_let(&mut self) -> Result<AstNodeId, ParseError> {
        self.expect(Token::Let)?;

        let name = if let Token::Ident(s) = self.next() {
            s
        } else {
            return Err(ParseError::UnexpectedToken(format!(
                "Unexpected identifier after let found, {:?}",
                self.peek()
            )));
        };

        self.expect(Token::Equal)?;

        let expression = self.parse_expr(0)?;
        self.expect(Token::Semicolon)?;

        let id = self.ast.len();
        self.ast.push(AstNode {
            kind: AstKind::Assign(name, expression),
        });

        Ok(id)
    }

    fn parse_primary(&mut self) -> Result<AstNodeId, ParseError> {
        match self.next() {
            Token::Number(n) => {
                let id = self.ast.len();
                self.ast.push(AstNode {
                    kind: AstKind::Number(n),
                });

                Ok(id)
            }
            Token::Ident(name) => {
                let id = self.ast.len();
                self.ast.push(AstNode {
                    kind: AstKind::Var(name),
                });

                Ok(id)
            }
            Token::LParen => {
                let expr = self.parse_expr(0)?;
                self.expect(Token::RParen)?;
                Ok(expr)
            }
            Token::RawString(raw) => {
                let id = self.ast.len();
                self.ast.push(AstNode {
                    kind: AstKind::RawString(raw),
                });

                Ok(id)
            }
            t => Err(ParseError::UnexpectedToken(format!(
                "Unexpected token in expression: {:?}",
                t
            ))),
        }
    }

    pub fn print_ast(&self, root: AstNodeId, indent: usize) {
        let pad = "  ".repeat(indent);
        let node = &self.ast[root];

        match &node.kind {
            AstKind::Number(n) => {
                println!("{pad}Number({n})");
            }
            AstKind::Var(sym) => {
                println!("{pad}Var({sym})");
            }
            AstKind::Assign(sym, rhs) => {
                println!("{pad}Assign({sym})");
                self.print_ast(*rhs, indent + 1);
            }
            AstKind::Add(lhs, rhs) => {
                println!("{pad}Add");
                self.print_ast(*lhs, indent + 1);
                self.print_ast(*rhs, indent + 1);
            }
            AstKind::Pow(lhs, rhs) => {
                println!("{pad}Pow");
                self.print_ast(*lhs, indent + 1);
                self.print_ast(*rhs, indent + 1);
            }
            AstKind::Div(lhs, rhs) => {
                println!("{pad}Div");
                self.print_ast(*lhs, indent + 1);
                self.print_ast(*rhs, indent + 1);
            }
            AstKind::Mul(lhs, rhs) => {
                println!("{pad}Mul");
                self.print_ast(*lhs, indent + 1);
                self.print_ast(*rhs, indent + 1);
            }
            AstKind::Sub(lhs, rhs) => {
                println!("{pad}Sub");
                self.print_ast(*lhs, indent + 1);
                self.print_ast(*rhs, indent + 1);
            }
            AstKind::RawString(sym) => {
                println!("{pad}RawString({sym})");
            }
            AstKind::Print(ast) => {
                println!("{pad}Print");
                self.print_ast(*ast, indent + 1);
            }
            AstKind::ReAssign(sym, rhs) => {
                println!("{pad}Reassign({sym})");
                self.print_ast(*rhs, indent + 1);
            }
        }
    }

    fn parse_print(&mut self) -> Result<AstNodeId, ParseError> {
        self.expect(Token::Print)?;
        self.expect(Token::LParen)?;

        let expr = self.parse_expr(0)?;

        self.expect(Token::RParen)?;
        self.expect(Token::Semicolon)?;

        let id = self.ast.len();

        self.ast.push(AstNode {
            kind: AstKind::Print(expr),
        });

        Ok(id)
    }

    fn parse_ident(&mut self) -> Result<AstNodeId, ParseError> {
        let name = if let Token::Ident(s) = self.next() {
            s
        } else {
            return Err(ParseError::UnexpectedToken(format!(
                "Unexpected identifier found, {:?}",
                self.peek()
            )));
        };

        self.expect(Token::Equal)?;

        let expression = self.parse_expr(0)?;
        self.expect(Token::Semicolon)?;

        let id = self.ast.len();
        self.ast.push(AstNode {
            kind: AstKind::ReAssign(name, expression),
        });

        Ok(id)
    }
}

fn precedence(op: &Token) -> Option<u8> {
    match op {
        Token::Star | Token::Slash => Some(2),
        Token::Plus | Token::Minus => Some(1),
        _ => None,
    }
}
