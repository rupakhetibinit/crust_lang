use std::iter::Peekable;

use logos::Logos;

use crate::lexer::Token;

#[derive(Debug)]
pub struct Parser<'p> {
    tokens: Peekable<logos::Lexer<'p, Token>>,
}

impl<'p> Parser<'p> {
    pub fn new(input: &'p str) -> Self {
        let lexer = Token::lexer(input);
        let tokens = lexer.peekable();
        Self { tokens }
    }

    pub fn peek(&mut self) -> Option<Result<Token, ()>> {
        self.tokens.peek().cloned()
    }

    pub fn next(&mut self) -> Option<Result<Token, ()>> {
        println!("Next token {:?}", self.tokens.peek());
        self.tokens.next()
    }

    pub fn parse_program(&mut self) -> Program {
        let mut body = Vec::new();
        while self.peek().is_some() {
            body.push(self.parse_top_level().unwrap());
        }
        Program { body }
    }

    pub fn parse_top_level(&mut self) -> Result<AstNode, String> {
        match self.peek() {
            Some(Ok(Token::Let)) => self.parse_let_statement(),
            Some(Ok(Token::Fn)) => self.parse_function_decl(),
            _ => Err("Unexpected end of input".to_string()),
        }
    }

    pub fn parse_function_decl(&mut self) -> Result<AstNode, String> {
        self.expect_token(Token::Fn)?;
        let name = self.expect_identifier()?;
        self.expect_token(Token::LeftParen)?;
        let params = self.parse_params()?;
        self.expect_token(Token::LBrace)?;

        let mut body_statements = Vec::new();
        while let Some(token) = self.peek() {
            match token {
                Ok(Token::RBrace) => break,
                _ => {
                    let stmt = self.parse_statement()?;
                    body_statements.push(stmt);

                    if let Some(Ok(Token::Semicolon)) = self.peek() {
                        self.next();
                    }
                }
            }
        }

        self.expect_token(Token::RBrace)?;

        let body = if body_statements.len() == 1 {
            body_statements.into_iter().next().unwrap()
        } else {
            AstNode::Expr(Expression::Block {
                body: body_statements,
                return_type: None,
            })
        };

        Ok(AstNode::Expr(Expression::FunctionDecl {
            name,
            params,
            body: Box::new(body),
            return_type: None,
        }))
    }

    fn parse_statement(&mut self) -> Result<AstNode, String> {
        match self.peek() {
            Some(Ok(Token::Let)) => {
                self.next();
                self.parse_let_statement()
            }
            Some(Ok(Token::Return)) => {
                self.next();
                let value = self.parse_expression(0)?;
                Ok(AstNode::Expr(Expression::Return {
                    expr: Box::new(value),
                }))
            }
            _ => self.parse_expression(0),
        }
    }

    pub fn parse_let_statement(&mut self) -> Result<AstNode, String> {
        let name = self.expect_identifier()?;

        let ty: Option<Type>;
        match self.peek() == Some(Ok(Token::Colon)) {
            true => {
                self.expect_token(Token::Colon)?;
                ty = Some(self.parse_type()?);
            }
            false => ty = None,
        };

        self.expect_token(Token::Equal)?;

        let value = self.parse_expression(0)?;

        self.expect_token(Token::Semicolon)?;

        Ok(AstNode::Expr(Expression::Let {
            var_name: name,
            value: Box::new(value),
            var_type: ty,
        }))
    }

    fn parse_type(&mut self) -> Result<Type, String> {
        match self.next() {
            Some(Ok(Token::Identifier(x))) => match x.as_str() {
                "i64" => Ok(Type::I64),
                "f64" => Ok(Type::F64),
                "u64" => Ok(Type::U64),
                _ => Err(format!("Unknown type: {}", x)),
            },
            x => Err(format!("Unexpected token {:?}", x)),
        }
    }

    fn parse_expression(&mut self, precedence: u8) -> Result<AstNode, String> {
        let mut left = self.parse_prefix()?;

        loop {
            let op = match self.peek() {
                Some(op) => op.unwrap(),
                None => break,
            };

            if let AstNode::Expr(Expression::Variable(ref name)) = left {
                if op == Token::LeftParen {
                    left = self.parse_function_call(name.clone())?;
                    continue;
                }
            }

            let (l_bp, r_bp) = match self.binding_power(op.clone()) {
                Some(x) => x,
                None => break,
            };

            if l_bp < precedence {
                break;
            }

            self.next().unwrap().unwrap();

            let right = self.parse_expression(r_bp)?;

            left = AstNode::Expr(Expression::Binary {
                left: Box::new(left),
                right: Box::new(right),
                operator: op,
                return_type: None,
            });
        }

        Ok(left)
    }

    fn parse_prefix(&mut self) -> Result<AstNode, String> {
        let token = self.next().unwrap().unwrap();
        match token {
            Token::Identifier(ident) => Ok(AstNode::Expr(Expression::Variable(ident))),
            Token::Integer(num) => Ok(AstNode::Expr(Expression::Literal(num))),
            Token::LeftParen => self.parse_grouped_expression(),
            Token::Pipe => self.parse_closure_function(),
            Token::Let => self.parse_let_statement(),
            Token::If => self.parse_if_statement(),
            Token::LBrace => {
                let expr = self.parse_expression(0)?;
                self.expect_token(Token::RBrace)?;
                Ok(expr)
            }
            Token::Return => self.parse_return_statement(),
            _ => Err(format!("Unexpected token in parse prefix {:?}", token)),
        }
    }

    fn parse_if_statement(&mut self) -> Result<AstNode, String> {
        let condition = self.parse_expression(0)?;

        self.expect_token(Token::LBrace)?;
        let then_branch = self.parse_expression(0)?;
        self.expect_token(Token::RBrace)?;

        let else_branch = if let Some(Ok(Token::Else)) = self.peek() {
            self.next();
            self.expect_token(Token::LBrace)?;
            let branch = self.parse_expression(0)?;
            self.expect_token(Token::RBrace)?;
            Some(Box::new(branch))
        } else {
            None
        };

        Ok(AstNode::Expr(Expression::If {
            condition: Box::new(condition),
            then_branch: Box::new(then_branch),
            else_branch,
        }))
    }

    fn parse_closure_function(&mut self) -> Result<AstNode, String> {
        let params = self.parse_closure_params()?;
        self.expect_token(Token::LBrace)?;
        let body = self.parse_expression(0)?;
        self.expect_token(Token::RBrace)?;
        Ok(AstNode::Expr(Expression::ClosureFunction {
            params: params,
            body: Box::new(body),
            return_type: None,
        }))
    }

    fn parse_params(&mut self) -> Result<Vec<Params>, String> {
        let mut params = Vec::new();
        while let Some(token) = self.next() {
            match token {
                Ok(Token::Identifier(ident)) => params.push(Params {
                    name: ident,
                    ty: None,
                }),
                Ok(Token::RightParen) => break,
                _ => return Err(format!("Unexpected token in parse params {:?}", token)),
            }
        }
        Ok(params)
    }

    fn parse_closure_params(&mut self) -> Result<Vec<Params>, String> {
        let mut params = Vec::new();
        while let Some(token) = self.next() {
            match token {
                Ok(Token::Identifier(ident)) => params.push(Params {
                    name: ident,
                    ty: None,
                }),
                Ok(Token::Pipe) => break,
                _ => return Err(format!("Unexpected token closure param {:?}", token)),
            }
        }
        Ok(params)
    }

    fn parse_grouped_expression(&mut self) -> Result<AstNode, String> {
        let expr = self.parse_expression(0)?;
        self.expect_token(Token::RightParen)?;
        Ok(expr)
    }

    fn expect_identifier(&mut self) -> Result<String, String> {
        let token = self.next().unwrap().unwrap();
        match token {
            Token::Identifier(ident) => Ok(ident),
            _ => Err(format!("Expected identifier, found {:?}", token)),
        }
    }

    fn expect_token(&mut self, token: Token) -> Result<Token, String> {
        match self.next() {
            Some(Ok(t)) if t == token => Ok(t),
            Some(Ok(t)) => Err(format!("Expected {:?}, found {:?}", token, t)),
            Some(Err(e)) => Err(format!("Error: {:?}", e)),
            None => Err("Unexpected end of input".to_string()),
        }
    }

    fn binding_power(&self, op: Token) -> Option<(u8, u8)> {
        match op {
            Token::Or => Some((1, 2)),
            Token::And => Some((3, 4)),
            Token::EqualEqual | Token::NotEqual => Some((5, 6)),
            Token::LessThan
            | Token::LessThanOrEqual
            | Token::GreaterThan
            | Token::GreaterThanOrEqual => Some((7, 8)),
            Token::Plus | Token::Minus => Some((9, 10)),
            Token::Star | Token::Slash => Some((11, 12)),
            Token::LeftParen => Some((30, 40)),
            _ => None,
        }
    }

    fn parse_function_call(&mut self, name: String) -> Result<AstNode, String> {
        self.expect_token(Token::LeftParen)?;

        let mut params = Vec::new();

        // Parse parameters until we hit a right paren
        while let Some(token) = self.peek() {
            match token {
                Ok(Token::RightParen) => break,
                _ => {
                    let param = self.parse_expression(0)?;

                    if let AstNode::Expr(expr) = param {
                        params.push(expr);
                    } else {
                        return Err("Expected expression in function call".to_string());
                    }

                    if let Some(Ok(Token::Comma)) = self.peek() {
                        self.next();
                    } else {
                        break;
                    }
                }
            }
        }

        self.expect_token(Token::RightParen)?;

        Ok(AstNode::Expr(Expression::FunctionCall {
            params,
            function_name: name,
        }))
    }

    fn parse_return_statement(&mut self) -> Result<AstNode, String> {
        self.expect_token(Token::Return)?;
        let expr = self.parse_expression(0)?;
        self.expect_token(Token::Semicolon)?;
        Ok(AstNode::Expr(Expression::Return {
            expr: Box::new(expr),
        }))
    }
}

#[derive(Debug)]
pub struct Program {
    body: Vec<AstNode>,
}

#[derive(Debug)]
pub enum AstNode {
    Expr(Expression),
}

#[derive(Debug)]
pub enum Expression {
    Binary {
        left: Box<AstNode>,
        operator: Token,
        right: Box<AstNode>,
        return_type: Option<Type>,
    },
    Let {
        var_name: String,
        value: Box<AstNode>,
        var_type: Option<Type>,
    },
    Block {
        body: Vec<AstNode>,
        return_type: Option<Type>,
    },
    If {
        condition: Box<AstNode>,
        then_branch: Box<AstNode>,
        else_branch: Option<Box<AstNode>>,
    },
    ClosureFunction {
        params: Vec<Params>,
        body: Box<AstNode>,
        return_type: Option<Type>,
    },
    FunctionCall {
        params: Vec<Expression>,
        function_name: String,
    },
    FunctionDecl {
        name: String,
        params: Vec<Params>,
        body: Box<AstNode>,
        return_type: Option<Type>,
    },
    Literal(i64),
    Variable(String),
    Return {
        expr: Box<AstNode>,
    },
}

#[derive(Debug)]
pub struct Params {
    name: String,
    ty: Option<Type>,
}

#[derive(Debug)]
pub enum Type {
    I64,
    U64,
    F64,
    Unit,
}

pub enum Operator {
    Math(BinOp),
    Logical(LogicalOp),
}

pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
}

pub enum LogicalOp {
    And,
    Or,
}
