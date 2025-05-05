use crate::{
    ast::{AstKind, AstNode, AstNodeId, Symbol},
    token::{self, Token},
};
use core::panic;
use slotmap::SlotMap;
use std::iter::Peekable;
use string_interner::{StringInterner, backend::StringBackend};

pub struct Parser<'a> {
    tokens: Peekable<std::vec::IntoIter<Token>>,
    interner: &'a mut StringInterner<StringBackend<Symbol>>,
    ast: &'a mut SlotMap<AstNodeId, AstNode>,
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

    fn expect(&mut self, expect: Token) {
        let next = self.next();

        assert_eq!(next, expect, "Expected {:?}, got {:?}", expect, next);
    }

    pub fn parse_stmt(&mut self) -> AstNodeId {
        match self.peek() {
            Token::Let => self.parse_let(),
            _ => self.parse_expr(),
        }
    }

    fn parse_expr(&mut self) -> AstNodeId {
        let mut left = self.parse_primary();

        loop {
            match self.peek() {
                Token::Plus => {
                    self.next(); // consume '+'
                    let right = self.parse_primary();
                    left = self.ast.insert(AstNode {
                        kind: AstKind::Add(left, right),
                    });
                }
                Token::Semicolon | Token::EOF | Token::RParen => break,
                unexpected => {
                    panic!("Unexpected token in expression {:?}", unexpected);
                }
            }
        }

        left
    }

    fn parse_let(&mut self) -> AstNodeId {
        self.expect(Token::Let);

        let name = if let Token::Ident(name) = self.next() {
            self.interner.get_or_intern(&name)
        } else {
            panic!("Expected identifier after Let")
        };

        self.expect(Token::Equal);

        let expr_id = self.parse_expr();
        self.expect(Token::Semicolon);

        self.ast.insert(AstNode {
            kind: AstKind::Assign(name, expr_id),
        })
    }

    fn parse_primary(&mut self) -> AstNodeId {
        match self.next() {
            Token::Number(n) => {
                return self.ast.insert(AstNode {
                    kind: AstKind::Number(n),
                });
            }
            Token::Ident(name) => {
                let sym = self.interner.get_or_intern(&name);
                self.ast.insert(AstNode {
                    kind: AstKind::Var(sym),
                })
            }
            Token::LParen => {
                let expr = self.parse_expr();
                self.expect(Token::RParen);
                expr
            }
            Token::Semicolon | Token::RParen | Token::EOF => {
                panic!("Unexpected token in primary expression: {:?}", self.peek());
            }
            t => panic!("Unexpected token in expression: {:?}", t),
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
        }
    }
}
