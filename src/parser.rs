use std::{env::current_exe, io::ErrorKind, ops::Range};

use crate::{
    error,
    lexer::{SpannedToken, Token},
};

#[derive(Debug, Clone)]
pub struct Program {
    pub items: Vec<Item>,
}

#[derive(Debug, Clone)]
pub enum Item {
    Function(Function),
    Struct(StructDef),
    Impl(ImplBlock),
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub params: Vec<Param>,
    pub return_type: Type,
    pub body: Block,
    pub span: Range<usize>,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub struct StructDef {
    pub name: String,
    pub fields: Vec<Field>,
}

#[derive(Debug, Clone)]
pub struct ImplBlock {
    pub struct_name: String,
    pub methods: Vec<Function>,
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: String,
    pub ty: Type,
    pub span: Range<usize>,
}

#[derive(Debug, Clone)]
pub struct Field {
    pub name: String,
    pub ty: Type,
    pub span: Range<usize>,
}

#[derive(Debug, Clone)]
pub enum Type {
    I64(Range<usize>),
    I32(Range<usize>),
    F64(Range<usize>),
    F32(Range<usize>),
    Bool(Range<usize>),
    Void(Range<usize>),
    String(Range<usize>),
    Custom(String, Range<usize>),
    Unit,
    Unknown,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Let { name: String, ty: Type, init: Expr },
    Return(Expr, Range<usize>),
    ExprStmt(Expr, Range<usize>),
}

#[derive(Debug, Clone)]
pub enum Expr {
    IntLiteral(i64, Type, Range<usize>),
    FloatLiteral(f64, Type, Range<usize>),
    BoolLiteral(bool, Type, Range<usize>),
    Var(String, Type, Range<usize>),
    Binary {
        op: BinOp,
        left: Box<Expr>,
        right: Box<Expr>,
        ty: Type,
        span: Range<usize>,
    },
    Call {
        func: Box<Expr>,
        args: Vec<Expr>,
        ty: Type,
        span: Range<usize>,
    },
    FieldAccess {
        base: Box<Expr>,
        field: String,
        ty: Type,
        span: Range<usize>,
    },
    Unary {
        op: UnaryOp,
        expr: Box<Expr>,
        ty: Type,
        span: Range<usize>,
    },
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Neg,
    Not,
}

#[derive(Debug, Clone)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Modulo,
    Or,
    And,
    Not,
    EqualEqual,
    LesserEqual,
    GreaterEqual,
    LessThan,
    GreaterThan,
    BitAnd,
    BitOr,
    NotEqual,
}

#[derive(Debug, Clone)]
pub struct Parser {
    tokens: Vec<SpannedToken>,
    pos: usize,
    pub filename: String,
    block_stack: Vec<Range<usize>>,
    current_span: Range<usize>,
}

#[derive(Debug, Clone)]
pub struct ParseError {
    pub kind: ParseErrorKind,
    pub span: Range<usize>,
    pub filename: String,
}

#[derive(Debug, Clone)]
pub enum ParseErrorKind {
    UnexpectedToken {
        found: Token,
        expected: Option<String>,
    },
    UnexpectedEOF {
        expected: Option<String>,
    },
    UnclosedBlock {
        open_span: Range<usize>,
    },
    ExpectedFunctionName {},
    InvalidType {
        found: Token,
    },
    ExpectedIdentAfterLet,
}

pub type ParseResult<T> = Result<T, ParseError>;

impl Parser {
    pub fn new(tokens: Vec<SpannedToken>, filename: String) -> Self {
        Parser {
            tokens,
            pos: 0,
            filename,
            block_stack: Vec::new(),
            current_span: 0..0,
        }
    }

    pub fn peek(&self) -> Option<&SpannedToken> {
        self.tokens.get(self.pos)
    }

    pub fn next(&mut self) -> Option<SpannedToken> {
        let token = self.tokens.get(self.pos).cloned();
        let tok = token.clone();
        if let Some(tok) = tok {
            self.pos += 1;
            self.current_span = tok.clone().span;
        }
        token
    }

    fn expect(&mut self, expected: Token) -> ParseResult<SpannedToken> {
        match self.next() {
            Some(tok) if tok.token == expected => Ok(tok),
            Some(tok) => Err(ParseError {
                kind: ParseErrorKind::UnexpectedToken {
                    found: tok.token.clone(),
                    expected: Some(expected.to_string()),
                },
                span: tok.span.clone(),
                filename: self.filename.clone(),
            }),
            None => Err(ParseError {
                kind: ParseErrorKind::UnexpectedEOF {
                    expected: Some(expected.to_string()),
                },
                span: self.current_span.clone(),
                filename: self.filename.clone(),
            }),
        }
    }

    pub fn parse_program(&mut self) -> ParseResult<Program> {
        let mut items = Vec::new();

        while self.peek().is_some() {
            items.push(self.parse_item()?)
        }

        Ok(Program { items })
    }

    pub fn parse_item(&mut self) -> ParseResult<Item> {
        match self.peek() {
            Some(tok) => match &tok.token {
                Token::Fn => {
                    self.next();
                    Ok(Item::Function(self.parse_function()?))
                }
                other => Err(ParseError {
                    kind: ParseErrorKind::UnexpectedToken {
                        found: other.clone(),
                        expected: Some("function, struct or impl".into()),
                    },
                    span: tok.span.clone(),
                    filename: self.filename.clone(),
                }),
            },
            None => Err(ParseError {
                kind: ParseErrorKind::UnexpectedEOF {
                    expected: Some("function, struct, or impl".to_string()),
                },
                span: self.current_span.clone(),
                filename: self.filename.clone(),
            }),
        }
    }

    pub fn parse_function(&mut self) -> ParseResult<Function> {
        let filename = self.filename.clone();
        let name_tok = self.next().ok_or(ParseError {
            kind: ParseErrorKind::ExpectedFunctionName {},
            span: self.current_span.clone(),
            filename,
        })?;

        let name = match &name_tok.token {
            Token::Ident(s) => s.clone(),
            _ => {
                return Err(ParseError {
                    kind: ParseErrorKind::ExpectedFunctionName {},
                    span: self.block_stack.last().cloned().unwrap(),
                    filename: self.filename.clone(),
                });
            }
        };

        self.expect(Token::LParen)?;
        let params = self.parse_params()?;
        self.expect(Token::RParen)?;

        self.expect(Token::Arrow)?;
        let return_type = self.parse_type()?;

        let body = self.parse_block()?;

        Ok(Function {
            name,
            params,
            return_type,
            body,
            span: name_tok.span.clone(),
        })
    }

    fn parse_params(&mut self) -> ParseResult<Vec<Param>> {
        let mut params = Vec::new();

        while let Some(tok) = self.peek() {
            match &tok.token {
                Token::Ident(_) => {
                    let name_tok = self.next().unwrap();
                    self.expect(Token::Colon)?;
                    let ty = self.parse_type()?;
                    params.push(Param {
                        name: match &name_tok.token {
                            Token::Ident(s) => s.clone(),
                            _ => unreachable!(),
                        },
                        ty,
                        span: name_tok.span.clone(),
                    });

                    if let Some(Token::Comma) = self.peek().map(|t| &t.token) {
                        self.next(); // consume comma
                    } else {
                        break;
                    }
                }
                _ => break,
            }
        }

        Ok(params)
    }

    fn parse_type(&mut self) -> ParseResult<Type> {
        let filename = self.filename.clone();
        let tok = self.next().ok_or(ParseError {
            kind: ParseErrorKind::UnexpectedEOF {
                expected: Some("type".into()),
            },
            span: self.current_span.clone(),
            filename,
        })?;

        match &tok.token {
            Token::Ident(s) => match s.as_str() {
                "i64" => Ok(Type::I64(tok.span.clone())),
                "i32" => Ok(Type::I32(tok.span.clone())),
                "f64" => Ok(Type::F64(tok.span.clone())),
                "f32" => Ok(Type::F32(tok.span.clone())),
                "bool" => Ok(Type::Bool(tok.span.clone())),
                "void" => Ok(Type::Void(tok.span.clone())),
                "String" => Ok(Type::String(tok.span.clone())),
                custom => Ok(Type::Custom(custom.into(), tok.span.clone())),
            },
            _ => Err(ParseError {
                kind: ParseErrorKind::InvalidType {
                    found: tok.token.clone(),
                },
                span: tok.span.clone(),
                filename: self.filename.clone(),
            }),
        }
    }

    fn is_eof(&self) -> bool {
        self.pos >= self.tokens.len()
    }

    fn parse_block(&mut self) -> ParseResult<Block> {
        let lbrace = self.expect(Token::LBrace)?;
        self.block_stack.push(lbrace.span.clone());

        let mut stmts = vec![];
        while !self.check(Token::RBrace) && !self.is_eof() {
            stmts.push(self.parse_stmt()?);
        }

        if self.is_eof() {
            let open_span = self.block_stack.pop().unwrap();

            return Err(ParseError {
                kind: ParseErrorKind::UnclosedBlock { open_span },
                span: lbrace.span.clone(),
                filename: self.filename.clone(),
            });
        }

        self.expect(Token::RBrace).map_err(|e| {
            let open_span = self.block_stack.pop().unwrap();

            let current_span = self
                .tokens
                .last()
                .map(|t| t.span.clone())
                .unwrap_or(open_span.clone());

            ParseError {
                kind: ParseErrorKind::UnclosedBlock {
                    open_span: current_span,
                },
                span: lbrace.span.clone(),
                filename: self.filename.clone(),
            }
        })?;

        self.block_stack.pop();

        Ok(Block { stmts })
    }

    fn parse_stmt(&mut self) -> ParseResult<Stmt> {
        match self.peek() {
            Some(tok) => match &tok.token {
                Token::Let => self.parse_let_stmt(),
                Token::Return => self.parse_return_stmt(),
                _ => self.parse_expr_stmt(),
            },
            None => Err(ParseError {
                kind: ParseErrorKind::UnexpectedEOF {
                    expected: Some("statement".to_string()),
                },
                span: self.current_span.clone(),
                filename: self.filename.clone(),
            }),
        }
    }

    fn parse_let_stmt(&mut self) -> ParseResult<Stmt> {
        let let_tok = self.next().unwrap();
        let name_tok = self.next().ok_or(ParseError {
            span: let_tok.span.clone(),
            filename: self.filename.clone(),
            kind: ParseErrorKind::ExpectedIdentAfterLet,
        })?;

        let name = match &name_tok.token {
            Token::Ident(s) => s.clone(),
            _ => {
                return Err(ParseError {
                    kind: ParseErrorKind::ExpectedIdentAfterLet,
                    span: name_tok.span.clone(),
                    filename: self.filename.clone(),
                });
            }
        };

        self.expect(Token::Equal)?;
        let init = self.parse_expr()?;
        self.expect(Token::SemiColon)?;

        Ok(Stmt::Let {
            name,
            ty: Type::Unknown,
            init,
        })
    }
    fn parse_return_stmt(&mut self) -> ParseResult<Stmt> {
        let return_tok = self.next().unwrap();
        let expr = self.parse_expr()?;
        self.expect(Token::SemiColon)?;
        Ok(Stmt::Return(expr, return_tok.span.clone()))
    }

    fn parse_expr_stmt(&mut self) -> ParseResult<Stmt> {
        let expr = self.parse_expr()?;
        let span = match &self.peek() {
            Some(tok) => tok.span.clone(),
            None => 0..0,
        };
        self.expect(Token::SemiColon)?;
        Ok(Stmt::ExprStmt(expr, span))
    }

    fn parse_expr(&mut self) -> ParseResult<Expr> {
        self.parse_logical_or()
    }

    fn parse_logical_or(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_logical_and()?;
        while self.check(Token::Or) {
            let op = self.next().unwrap();

            let rhs = self.parse_logical_and()?;
            expr = Expr::Binary {
                op: BinOp::Or,
                left: Box::new(expr),
                right: Box::new(rhs),
                ty: Type::Unknown,
                span: op.span.clone(),
            }
        }

        Ok(expr)
    }

    fn parse_logical_and(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_equality()?;
        while self.check(Token::And) {
            let op = self.next().unwrap();

            let rhs = self.parse_equality()?;
            expr = Expr::Binary {
                op: BinOp::And,
                left: Box::new(expr),
                right: Box::new(rhs),
                ty: Type::Unknown,
                span: op.span.clone(),
            }
        }

        Ok(expr)
    }

    fn parse_equality(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_comparison()?;
        while self.check(Token::EqualEqual) || self.check(Token::NotEqual) {
            let op = self.next().unwrap();
            let rhs = self.parse_comparison()?;
            let binop = match op.token {
                Token::EqualEqual => BinOp::EqualEqual,
                Token::NotEqual => BinOp::NotEqual,
                _ => unreachable!(),
            };
            expr = Expr::Binary {
                op: binop,
                left: Box::new(expr),
                right: Box::new(rhs),
                ty: Type::Unknown,
                span: op.span.clone(),
            };
        }
        Ok(expr)
    }

    fn parse_comparison(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_add_sub()?;
        while self.check(Token::LeftAngleBracket)
            || self.check(Token::RightAngleBracket)
            || self.check(Token::LesserEqual)
            || self.check(Token::GreaterEqual)
        {
            let op = self.next().unwrap();
            let rhs = self.parse_add_sub()?;
            let binop = match op.token {
                Token::LeftAngleBracket => BinOp::LessThan,
                Token::RightAngleBracket => BinOp::GreaterThan,
                Token::LesserEqual => BinOp::LesserEqual,
                Token::GreaterEqual => BinOp::GreaterEqual,
                _ => unreachable!(),
            };
            expr = Expr::Binary {
                op: binop,
                left: Box::new(expr),
                right: Box::new(rhs),
                ty: Type::Unknown,
                span: op.span.clone(),
            };
        }
        Ok(expr)
    }

    fn parse_add_sub(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_mul_div()?;
        while self.check(Token::Plus) || self.check(Token::Minus) {
            let op = self.next().unwrap();
            let rhs = self.parse_mul_div()?;
            let binop = match op.token {
                Token::Plus => BinOp::Add,
                Token::Minus => BinOp::Sub,
                _ => unreachable!(),
            };
            expr = Expr::Binary {
                op: binop,
                left: Box::new(expr),
                right: Box::new(rhs),
                ty: Type::Unknown,
                span: op.span.clone(),
            };
        }
        Ok(expr)
    }

    fn parse_mul_div(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_unary()?;
        while self.check(Token::Star) || self.check(Token::Slash) || self.check(Token::Modulo) {
            let op = self.next().unwrap();
            let rhs = self.parse_unary()?;
            let binop = match op.token {
                Token::Star => BinOp::Mul,
                Token::Slash => BinOp::Div,
                Token::Modulo => BinOp::Modulo,
                _ => unreachable!(),
            };
            expr = Expr::Binary {
                op: binop,
                left: Box::new(expr),
                right: Box::new(rhs),
                ty: Type::Unknown,
                span: op.span.clone(),
            };
        }
        Ok(expr)
    }

    fn parse_unary(&mut self) -> ParseResult<Expr> {
        if self.check(Token::Minus) || self.check(Token::Not) {
            let op = self.next().unwrap();
            let rhs = self.parse_unary()?;
            return Ok(Expr::Unary {
                op: match op.token {
                    Token::Minus => UnaryOp::Neg,
                    Token::Not => UnaryOp::Not,
                    _ => unreachable!(),
                },
                expr: Box::new(rhs),
                ty: Type::Unknown,
                span: op.span.clone(),
            });
        }
        self.parse_call()
    }
    fn parse_call(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_primary()?;
        loop {
            if self.check(Token::LParen) {
                let lparen = self.next().unwrap();
                let mut args = vec![];
                if !self.check(Token::RParen) {
                    loop {
                        args.push(self.parse_expr()?);
                        if !self.check(Token::Comma) {
                            break;
                        }
                        self.next();
                    }
                }
                self.expect(Token::RParen)?;
                expr = Expr::Call {
                    func: Box::new(expr),
                    args,
                    ty: Type::Unknown,
                    span: lparen.span.clone(),
                };
            } else if self.check(Token::Dot) {
                let dot = self.next().unwrap();
                let field = self.expect_ident()?;
                expr = Expr::FieldAccess {
                    base: Box::new(expr),
                    field,
                    ty: Type::Unknown,
                    span: dot.span.clone(),
                };
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn parse_primary(&mut self) -> ParseResult<Expr> {
        let tok = self.next().ok_or(ParseError {
            kind: ParseErrorKind::UnexpectedEOF {
                expected: Some("expression".into()),
            },
            span: self.current_span.clone(),
            filename: self.filename.clone(),
        })?;
        match tok.token {
            Token::Int(i) => Ok(Expr::IntLiteral(i, Type::I64(tok.span.clone()), tok.span)),
            Token::Float(f) => Ok(Expr::FloatLiteral(f, Type::F64(tok.span.clone()), tok.span)),
            Token::Ident(name) => Ok(Expr::Var(name, Type::Unknown, tok.span.clone())),
            Token::LParen => {
                let expr = self.parse_expr()?;
                self.expect(Token::RParen)?;
                Ok(expr)
            }
            other => Err(ParseError {
                kind: ParseErrorKind::UnexpectedToken {
                    found: other.clone(),
                    expected: Some("expression".into()),
                },
                span: tok.span.clone(),
                filename: self.filename.clone(),
            }),
        }
    }

    fn check(&self, kind: Token) -> bool {
        matches!(self.tokens.get(self.pos).map(|t| &t.token), Some(k) if *k == kind)
    }

    fn expect_ident(&mut self) -> ParseResult<String> {
        let tok = self.next().ok_or(ParseError {
            kind: ParseErrorKind::UnexpectedEOF {
                expected: Some("identifier".into()),
            },
            span: self.current_span.clone(),
            filename: self.filename.clone(),
        })?;

        match tok.token {
            Token::Ident(name) => Ok(name),
            _ => Err(ParseError {
                kind: ParseErrorKind::UnexpectedEOF {
                    expected: Some("identifier".into()),
                },
                span: tok.span.clone(),
                filename: self.filename.clone(),
            }),
        }
    }
}
