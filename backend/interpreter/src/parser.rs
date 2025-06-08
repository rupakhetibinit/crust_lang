use crate::{
    ast::{AstNode, AstNodeId, BinOp, ComparisonOp},
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

    fn peek_nth(&self, n: usize) -> Option<Token> {
        self.tokens.clone().nth(n)
    }

    #[allow(unused)]
    fn peek_two(&self) -> (Option<Token>, Option<Token>) {
        (self.peek_nth(0), self.peek_nth(1))
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
            Token::If => self.parse_if()?,
            Token::Print => self.parse_print()?,
            Token::Fn => self.parse_fn_decl()?,
            Token::Return => self.parse_return()?,

            Token::Ident(_) => match self.peek_nth(1) {
                Some(Token::Equal) => self.parse_ident()?,
                Some(Token::LParen) => {
                    let call = self.parse_fn_call()?;
                    self.expect(Token::Semicolon)?;
                    call
                }
                Some(Token::Dot) => {
                    let base_expr_id = self.parse_expr(0)?;

                    if self.peek() == &Token::Equal {
                        let ast_node = self.ast[base_expr_id].clone();
                        if let AstNode::StructFieldAccess { instance, field } = ast_node {
                            self.next();

                            let value = self.parse_expr(0)?;
                            self.expect(Token::Semicolon)?;

                            let assignment_id = self.ast.len();

                            self.ast.push(AstNode::StructFieldAssignment {
                                instance,
                                field,
                                value,
                            });

                            assignment_id
                        } else {
                            return Err(ParseError::UnexpectedToken(format!(
                                "Expected struct field access, got {:?}",
                                ast_node
                            )));
                        }
                    } else {
                        self.expect(Token::Semicolon)?;
                        base_expr_id
                    }
                }
                _ => {
                    let expr = self.parse_expr(0)?;
                    self.expect(Token::Semicolon)?;
                    expr
                }
            },
            Token::For => self.parse_for_loop()?,
            Token::LBrace => {
                self.next();

                let mut block_items = Vec::new();

                while !matches!(self.peek(), Token::RBrace) {
                    let stmt = self.parse_stmt()?;
                    block_items.push(stmt);
                }
                self.expect(Token::RBrace)?;

                let block_id = self.ast.len();

                self.ast.push(AstNode::Block(block_items));

                block_id
            }
            Token::StructDecl => self.parse_struct_decl()?,
            Token::StructImpl => self.parse_struct_impl()?,
            _ => {
                let expr = self.parse_expr(0)?;
                expr
            }
        };

        Ok(node_id)
    }

    fn parse_for_loop(&mut self) -> Result<usize, ParseError> {
        self.expect(Token::For)?;
        self.expect(Token::LParen)?;
        let ast = self.parse_let()?;
        let condition = self.parse_expr(0)?;
        self.expect(Token::Semicolon)?;
        let increment = if let Ok(incr_id) = self.parse_post_increment() {
            incr_id
        } else {
            self.parse_expr(0)?
        };
        self.expect(Token::RParen)?;
        self.expect(Token::LBrace)?;
        let mut body = Vec::new();
        while !matches!(self.peek(), Token::RBrace) {
            let stmt = self.parse_stmt()?;
            body.push(stmt);
        }
        self.expect(Token::RBrace)?;
        let id = self.ast.len();
        self.ast.push(AstNode::ForLoop {
            init: ast,
            condition,
            increment,
            body,
        });
        Ok(id)
    }

    fn parse_expr(&mut self, min_prec: u8) -> Result<AstNodeId, ParseError> {
        let mut left = self.parse_primary()?;

        while let Some(prec) = precedence(self.peek()) {
            if prec < min_prec {
                break;
            }

            let op = self.next();

            let right = self.parse_expr(prec + 1)?;

            let node = match op {
                Token::Plus => AstNode::BinaryExpression(left, BinOp::Add, right),
                Token::Minus => AstNode::BinaryExpression(left, BinOp::Sub, right),
                Token::Star => AstNode::BinaryExpression(left, BinOp::Mul, right),
                Token::Slash => AstNode::BinaryExpression(left, BinOp::Div, right),
                Token::Caret => AstNode::BinaryExpression(left, BinOp::Exp, right),
                Token::EqualEqual => AstNode::Equality(left, right),
                Token::LessThan => AstNode::Comparison(left, ComparisonOp::LessOrEqual, right),
                Token::GreaterThan => {
                    AstNode::Comparison(left, ComparisonOp::GreaterOrEqual, right)
                }
                Token::Less => AstNode::Comparison(left, ComparisonOp::Less, right),
                Token::Greater => AstNode::Comparison(left, ComparisonOp::Greater, right),
                Token::And => AstNode::Comparison(left, ComparisonOp::And, right),
                Token::Or => AstNode::Comparison(left, ComparisonOp::Or, right),
                Token::Modulo => AstNode::BinaryExpression(left, BinOp::Modulo, right),
                _ => return Err(ParseError::Unreachable),
            };

            let id = self.ast.len();

            self.ast.push(node);

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
        self.ast.push(AstNode::Assign(name, expression));

        Ok(id)
    }

    fn parse_primary(&mut self) -> Result<AstNodeId, ParseError> {
        if let Some(Token::Ident(_)) = self.peek_nth(0) {
            if self.peek_nth(1) == Some(Token::LParen) {
                return self.parse_fn_call();
            }
        }
        let mut left = match self.next() {
            Token::Number(n) => {
                let id = self.ast.len();
                self.ast.push(AstNode::Number(n));

                Ok(id)
            }
            Token::Ident(name) => {
                if matches!(self.peek(), Token::DoubleColon) {
                    return self.parse_struct_static_call(name);
                }

                if matches!(self.peek(), Token::LBrace) {
                    return self.parse_struct_init(name);
                }

                let id = self.ast.len();
                self.ast.push(AstNode::Var(name));

                Ok(id)
            }
            Token::LParen => {
                let expr = self.parse_expr(0)?;
                self.expect(Token::RParen)?;
                Ok(expr)
            }
            Token::RawString(raw) => {
                let id = self.ast.len();
                self.ast.push(AstNode::RawString(raw));

                Ok(id)
            }
            t => Err(ParseError::UnexpectedToken(format!(
                "Unexpected token in expression: {:?}",
                t
            ))),
        }?;

        while matches!(self.peek(), Token::Dot) {
            self.next();

            let name = if let Token::Ident(s) = self.next() {
                s
            } else {
                return Err(ParseError::UnexpectedToken(format!(
                    "Expected field or method name after dot, got {:?}",
                    self.peek()
                )));
            };

            if matches!(self.peek(), Token::LParen) {
                self.next();

                let mut args = vec![];

                if !matches!(self.peek(), Token::RParen) {
                    loop {
                        let expr_id = self.parse_expr(0)?;
                        args.push(expr_id);

                        if !matches!(self.peek(), Token::Comma) {
                            break;
                        }

                        self.next();
                    }
                }

                self.expect(Token::RParen)?;

                let id = self.ast.len();

                self.ast.push(AstNode::StructMethodCall {
                    struct_instance: left,
                    method_name: name,
                    args,
                });

                left = id;
            } else {
                let id = self.ast.len();
                self.ast.push(AstNode::StructFieldAccess {
                    instance: left,
                    field: name,
                });
                left = id;
            }
        }
        return Ok(left);
    }

    fn parse_print(&mut self) -> Result<AstNodeId, ParseError> {
        self.expect(Token::Print)?;
        self.expect(Token::LParen)?;

        let mut exprs = Vec::<AstNodeId>::new();
        let expr = self.parse_expr(0)?;
        exprs.push(expr);

        while matches!(self.peek(), Token::Comma) {
            self.next();
            let expr = self.parse_expr(0)?;
            exprs.push(expr);
        }

        self.expect(Token::RParen)?;
        self.expect(Token::Semicolon)?;

        let id = self.ast.len();

        self.ast.push(AstNode::Print(exprs));

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
        self.ast.push(AstNode::Reassignment(name, expression));

        Ok(id)
    }

    fn parse_fn_decl(&mut self) -> Result<AstNodeId, ParseError> {
        self.expect(Token::Fn)?;

        let name = if let Token::Ident(s) = self.next() {
            s
        } else {
            return Err(ParseError::UnexpectedToken(format!(
                "Unexpected identifier found, {:?}",
                self.peek()
            )));
        };

        self.expect(Token::LParen)?;

        let mut args = Vec::new();

        while let Token::Ident(arg) = self.peek() {
            args.push(arg.clone());

            self.next();

            if matches!(self.peek(), Token::Comma) {
                self.next();
            } else {
                break;
            }
        }

        self.expect(Token::RParen)?;
        self.expect(Token::LBrace)?;

        let mut body = Vec::new();

        while !matches!(self.peek(), Token::RBrace) {
            let stmt = self.parse_stmt()?;
            body.push(stmt);
        }

        self.expect(Token::RBrace)?;

        let id = self.ast.len();

        let node = AstNode::FunctionDeclaration {
            name,
            args,
            block: body,
        };

        self.ast.push(node);

        Ok(id)
    }

    fn parse_if(&mut self) -> Result<AstNodeId, ParseError> {
        self.expect(Token::If)?;

        let expression = self.parse_expr(0)?;

        self.expect(Token::LBrace)?;

        let mut body = Vec::new();

        while !matches!(self.peek(), Token::RBrace) {
            let stmt = self.parse_stmt()?;
            body.push(stmt);
        }

        self.expect(Token::RBrace)?;

        let mut else_block = Vec::new();
        let mut else_if_blocks = Some(Vec::<(AstNodeId, Vec<AstNodeId>)>::new());

        while let Token::ElseIf = self.peek() {
            self.expect(Token::ElseIf)?;

            let else_if_expr = self.parse_expr(0)?;

            self.expect(Token::LBrace)?;

            let mut else_if_body = Vec::new();

            while !matches!(self.peek(), Token::RBrace) {
                let stmt = self.parse_stmt()?;
                else_if_body.push(stmt);
            }

            self.expect(Token::RBrace)?;

            if let Some(ref mut blocks) = else_if_blocks {
                blocks.push((else_if_expr, else_if_body));
            } else {
                else_if_blocks = Some(vec![(else_if_expr, else_if_body)]);
            }
        }

        if let Token::Else = self.peek() {
            self.expect(Token::Else)?;

            self.expect(Token::LBrace)?;

            while !matches!(self.peek(), Token::RBrace) {
                let stmt = self.parse_stmt()?;

                else_block.push(stmt);
            }

            self.expect(Token::RBrace)?;
        }

        let id = self.ast.len();

        let node = AstNode::If {
            expression,
            block: body,
            else_if_blocks,
            else_block: Some(else_block),
        };

        self.ast.push(node);

        Ok(id)
    }

    fn parse_return(&mut self) -> Result<AstNodeId, ParseError> {
        self.expect(Token::Return)?;

        let expr = self.parse_expr(0)?;

        self.expect(Token::Semicolon)?;

        let id = self.ast.len();

        let node = AstNode::Return(expr);

        self.ast.push(node);

        Ok(id)
    }

    fn parse_fn_call(&mut self) -> Result<AstNodeId, ParseError> {
        let name = if let Token::Ident(s) = self.next() {
            s
        } else {
            return Err(ParseError::UnexpectedToken(format!(
                "Unexpected identifier found, {:?}",
                self.peek()
            )));
        };

        let func_id: usize = {
            let id = self.ast.len();
            self.ast.push(AstNode::Var(name.clone()));
            id
        };

        self.expect(Token::LParen)?;

        let mut args = vec![];

        if !matches!(self.peek(), Token::RParen) {
            {
                loop {
                    let expr_id = self.parse_expr(0)?;
                    args.push(expr_id);

                    if matches!(self.peek(), Token::Comma) {
                        self.next();
                    } else {
                        break;
                    }
                }
            }
        }

        self.expect(Token::RParen)?;

        let call_id = self.ast.len();

        self.ast.push(AstNode::FunctionCall {
            func: func_id,
            args,
        });

        Ok(call_id)
    }

    fn parse_post_increment(&mut self) -> Result<AstNodeId, ParseError> {
        let Token::Ident(name) = self.next() else {
            return Err(ParseError::UnexpectedToken(format!(
                "Expected identifier for post increment, got {:?}",
                self.peek()
            )));
        };

        self.expect(Token::Increment)?;

        let id = self.ast.len();

        self.ast.push(AstNode::PostIncrement(name.clone()));

        Ok(id)
    }

    fn parse_struct_decl(&mut self) -> Result<AstNodeId, ParseError> {
        self.expect(Token::StructDecl)?;

        let name = if let Token::Ident(s) = self.next() {
            s
        } else {
            return Err(ParseError::UnexpectedToken(format!(
                "Expected identifier for struct declaration, got {:?}",
                self.peek()
            )));
        };

        self.expect(Token::LBrace)?;

        let mut fields = Vec::new();

        while !matches!(self.peek(), Token::RBrace) {
            let field_name = if let Token::Ident(s) = self.next() {
                s
            } else {
                return Err(ParseError::UnexpectedToken(format!(
                    "Expected identifier for struct field, got {:?}",
                    self.peek()
                )));
            };

            self.expect(Token::Comma)?;

            fields.push(field_name);

            if matches!(self.peek(), Token::Comma) {
                self.next();
            }
        }

        self.expect(Token::RBrace)?;

        let id = self.ast.len();

        self.ast.push(AstNode::StructDeclaration { name, fields });

        Ok(id)
    }

    fn parse_struct_impl(&mut self) -> Result<AstNodeId, ParseError> {
        self.expect(Token::StructImpl)?;

        let name = if let Token::Ident(s) = self.next() {
            s
        } else {
            return Err(ParseError::UnexpectedToken(format!(
                "Expected identifier for struct implementation, got {:?}",
                self.peek()
            )));
        };

        self.expect(Token::LBrace)?;

        let mut methods = Vec::new();

        while !matches!(self.peek(), Token::RBrace) {
            let method_id = self.parse_struct_method()?;
            methods.push(method_id);

            if matches!(self.peek(), Token::Comma) {
                self.next();
            }
        }

        self.expect(Token::RBrace)?;
        let id = self.ast.len();

        self.ast.push(AstNode::StructImpl { name, methods });

        Ok(id)
    }

    fn parse_struct_method(&mut self) -> Result<AstNodeId, ParseError> {
        self.expect(Token::Fn)?;
        let is_static = if let Some(Token::Ident(_)) = self.peek_nth(0) {
            if let Some(Token::LParen) = self.peek_nth(1) {
                let mut temp_tokens = self.tokens.clone();
                temp_tokens.next();
                temp_tokens.next();

                if let Some(Token::Ident(first_param)) = temp_tokens.next() {
                    first_param != "self"
                } else {
                    true
                }
            } else {
                return Err(ParseError::UnexpectedToken(format!(
                    "Expected '(' after method name, got {:?}",
                    self.peek_nth(1)
                )));
            }
        } else {
            return Err(ParseError::UnexpectedToken(format!(
                "Expected method name, got {:?}",
                self.peek()
            )));
        };

        let name = if let Token::Ident(s) = self.next() {
            s
        } else {
            return Err(ParseError::UnexpectedToken(format!(
                "Expected method name, got {:?}",
                self.peek()
            )));
        };

        self.expect(Token::LParen)?;

        let mut params = Vec::new();

        if !matches!(self.peek(), Token::RParen) {
            loop {
                if let Token::Ident(param) = self.next() {
                    params.push(param);
                } else {
                    return Err(ParseError::UnexpectedToken(format!(
                        "Expected parameter name, got {:?}",
                        self.peek()
                    )));
                }

                if matches!(self.peek(), Token::Comma) {
                    self.next();
                } else {
                    break;
                }
            }
        }

        self.expect(Token::RParen)?;
        self.expect(Token::LBrace)?;

        let mut body = Vec::new();
        while !matches!(self.peek(), Token::RBrace) {
            let stmt = self.parse_stmt()?;
            body.push(stmt);
        }

        self.expect(Token::RBrace)?;

        let id = self.ast.len();

        self.ast.push(AstNode::StructMethod {
            name,
            params,
            body,
            is_static,
        });

        Ok(id)
    }

    fn parse_struct_static_call(&mut self, struct_name: String) -> Result<AstNodeId, ParseError> {
        self.expect(Token::DoubleColon)?;

        let method_name = if let Token::Ident(s) = self.next() {
            s
        } else {
            return Err(ParseError::UnexpectedToken(format!(
                "Expected identifier for struct method call, got {:?}",
                self.peek()
            )));
        };

        self.expect(Token::LParen)?;

        let mut args = vec![];

        if !matches!(self.peek(), Token::RParen) {
            loop {
                let expr_id = self.parse_expr(0)?;
                args.push(expr_id);

                if matches!(self.peek(), Token::Comma) {
                    self.next();
                } else {
                    break;
                }
            }
        }

        self.expect(Token::RParen)?;

        let id = self.ast.len();

        self.ast.push(AstNode::StructStaticCall {
            struct_name,
            method_name,
            args,
        });

        return Ok(id);
    }

    fn parse_struct_init(&mut self, struct_name: String) -> Result<AstNodeId, ParseError> {
        self.expect(Token::LBrace)?;

        let mut fields = Vec::new();

        while !matches!(self.peek(), Token::RBrace) {
            let field_name = if let Token::Ident(s) = self.next() {
                s
            } else {
                return Err(ParseError::UnexpectedToken(format!(
                    "Expected identifier for struct field, got {:?}",
                    self.peek()
                )));
            };

            self.expect(Token::Equal)?;

            let value = self.parse_expr(0)?;

            fields.push((field_name, value));

            if matches!(self.peek(), Token::Comma) {
                self.next();
            }
        }

        self.expect(Token::RBrace)?;

        let id = self.ast.len();

        self.ast.push(AstNode::StructInit {
            struct_name,
            fields,
        });

        Ok(id)
    }
}

fn precedence(op: &Token) -> Option<u8> {
    match op {
        Token::Star | Token::Slash | Token::Modulo => Some(4),
        Token::Plus | Token::Minus => Some(3),
        Token::EqualEqual
        | Token::Return
        | Token::Less
        | Token::Greater
        | Token::GreaterThan
        | Token::LessThan => Some(2),
        Token::And => Some(1),
        Token::Or => Some(0),
        _ => None,
    }
}
