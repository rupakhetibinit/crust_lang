use crate::{
    ast::{AstKind, AstNode, AstNodeId, Symbol},
    token::Token,
};

use slotmap::SlotMap;
use std::iter::Peekable;
use string_interner::{StringInterner, backend::StringBackend};

pub struct Parser<'a> {
    tokens: Peekable<std::vec::IntoIter<Token>>,
    interner: &'a mut StringInterner<StringBackend<Symbol>>,
    ast: &'a mut SlotMap<AstNodeId, AstNode>,
}

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken(String),
    Unreachable,
}

impl<'a> Parser<'a> {
    pub fn new(
        tokens: Vec<Token>,
        interner: &'a mut StringInterner<StringBackend<Symbol>>,
        ast: &'a mut SlotMap<AstNodeId, AstNode>,
    ) -> Self {
        Self {
            tokens: tokens.into_iter().peekable(),
            interner,
            ast,
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

    pub fn parse_stmt(&mut self) -> Result<AstNodeId, ParseError> {
        match self.peek() {
            Token::Let => Ok(self.parse_let()?),
            _ => {
                let expr = self.parse_expr(0)?;
                self.expect(Token::Semicolon)?;
                Ok(expr)
            }
        }
    }

    fn parse_expr(&mut self, min_prec: u8) -> Result<AstNodeId, ParseError> {
        let mut left = self.parse_primary()?;

        while let Some(prec) = precedence(self.peek()) {
            if prec < min_prec {
                break;
            }

            let op = self.next();

            let right = self.parse_expr(prec + 1)?;

            left = self.ast.insert(match op {
                Token::Caret => AstNode {
                    kind: AstKind::Pow(left, right),
                },
                Token::Plus => AstNode {
                    kind: AstKind::Add(left, right),
                },
                Token::Minus => AstNode {
                    kind: AstKind::Sub(left, right),
                },
                Token::Star => AstNode {
                    kind: AstKind::Mul(left, right),
                },
                Token::Slash => AstNode {
                    kind: AstKind::Div(left, right),
                },
                _ => Err(ParseError::Unreachable)?,
            })
        }

        Ok(left)
    }

    fn parse_let(&mut self) -> Result<AstNodeId, ParseError> {
        self.expect(Token::Let)?;

        let name = if let Token::Ident(name) = self.next() {
            self.interner.get_or_intern(&name)
        } else {
            return Err(ParseError::UnexpectedToken(format!(
                "Unexpected identifier after let found, {:?}",
                self.peek()
            )));
        };

        self.expect(Token::Equal)?;

        let expression = self.parse_expr(0)?;
        self.expect(Token::Semicolon)?;

        Ok(self.ast.insert(AstNode {
            kind: AstKind::Assign(name, expression),
        }))
    }

    fn parse_primary(&mut self) -> Result<AstNodeId, ParseError> {
        match self.next() {
            Token::Number(n) => {
                return Ok(self.ast.insert(AstNode {
                    kind: AstKind::Number(n),
                }));
            }
            Token::Ident(name) => {
                let sym = self.interner.get_or_intern(&name);
                Ok(self.ast.insert(AstNode {
                    kind: AstKind::Var(sym),
                }))
            }
            Token::LParen => {
                let expr = self.parse_expr(0)?;
                self.expect(Token::RParen)?;
                Ok(expr)
            }
            Token::RawString(raw) => {
                let sym = self.interner.get_or_intern(raw);
                Ok(self.ast.insert(AstNode {
                    kind: AstKind::RawString(sym),
                }))
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
                let name = self.interner.resolve(*sym).unwrap();
                println!("{pad}Var({name})");
            }
            AstKind::Assign(sym, rhs) => {
                let name = self.interner.resolve(*sym).unwrap();
                println!("{pad}Assign({name})");
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
                let string = self.interner.resolve(*sym).unwrap();

                println!("{pad}RawString({string})");
            }
        }
    }
}

fn precedence(op: &Token) -> Option<u8> {
    match op {
        Token::Star | Token::Slash => Some(2),
        Token::Plus | Token::Minus => Some(1),
        _ => None,
    }
}
