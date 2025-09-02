use std::{io::ErrorKind, ops::Range};

use crate::lexer::{SpannedToken, Token};

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
}

#[derive(Debug)]
pub struct ParseError {
    pub message: String,
    pub span: Range<usize>,
    pub filename: String,
    pub note: Option<String>,
    pub secondary_span: Option<(String, Range<usize>)>,
    pub label: Option<String>,
    pub hint: Option<(String, Range<usize>)>,
}

pub type ParseResult<T> = Result<T, ParseError>;

impl Parser {
    pub fn new(tokens: Vec<SpannedToken>, filename: String) -> Self {
        Parser {
            tokens,
            pos: 0,
            filename,
            block_stack: Vec::new(),
        }
    }

    pub fn peek(&self) -> Option<&SpannedToken> {
        self.tokens.get(self.pos)
    }

    pub fn next(&mut self) -> Option<SpannedToken> {
        let tok = self.tokens.get(self.pos).cloned();
        if tok.is_some() {
            self.pos += 1;
        }
        tok
    }

    pub fn expect(&mut self, kind: Token) -> ParseResult<SpannedToken> {
        let filename = self.filename.clone();
        match self.next() {
            Some(tok) if tok.token == kind => Ok(tok),
            Some(tok) => Err(ParseError {
                message: format!("Expected {:?}, found {:?}", kind, tok.token),
                span: tok.span.clone(),
                filename: filename.clone(),
                note: None,
                secondary_span: None,
                label: None,
                hint: None,
            }),
            None => Err(ParseError {
                message: format!("Expected {:?}, found EOF", kind),
                span: 0..0,
                filename: filename.clone(),
                note: None,
                secondary_span: None,
                label: None,
                hint: None,
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
                _ => Err(ParseError {
                    message: format!("Unexpected token in program: {:?}", tok.token),
                    span: tok.span.clone(),
                    secondary_span: None,
                    note: None,
                    filename: self.filename.clone(),
                    label: None,
                    hint: None,
                }),
            },
            None => Err(ParseError {
                message: "Unexpected end of file".to_string(),
                span: 0..0,
                filename: self.filename.clone(),
                note: None,
                secondary_span: None,
                label: None,
                hint: None,
            }),
        }
    }

    pub fn parse_function(&mut self) -> ParseResult<Function> {
        let filename = self.filename.clone();
        let name_tok = self.next().ok_or(ParseError {
            message: format!("Expected function name"),
            span: 0..0,
            filename: filename.clone(),
            note: None,
            secondary_span: None,
            label: None,
            hint: None,
        })?;

        let name = match &name_tok.token {
            Token::Ident(s) => s.clone(),
            _ => {
                return Err(ParseError {
                    message: format!("Expected function name"),
                    span: name_tok.span.clone(),
                    filename: filename.clone(),
                    note: None,
                    secondary_span: None,
                    label: None,
                    hint: None,
                });
            }
        };

        self.expect(Token::LParen)?;
        let params = self.parse_params()?;
        self.expect(Token::RParen)?;

        self.expect(Token::Arrow)?;
        let return_type = self.parse_type()?;

        let body = self.parse_block()?;

        self.expect(Token::RBrace)?;

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
            message: "Expected type".to_string(),
            span: 0..0,
            filename: filename.clone(),
            secondary_span: None,
            note: None,
            label: None,
            hint: None,
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
                message: format!("Expected type annotation, got {:?}", tok.token.to_string()),
                span: tok.span.clone(),
                filename: filename.clone(),
                secondary_span: None,
                note: None,
                label: None,
                hint: None,
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
            // ERROR: unclosed block
            let open_span = self.block_stack.pop().unwrap();

            let current_span = self
                .tokens
                .last()
                .map(|t| t.span.clone())
                .unwrap_or(open_span.clone());

            return Err(ParseError {
                message: "this file contains an unclosed block".to_string(),
                span: lbrace.span.clone(), // where we noticed
                secondary_span: Some((
                    "Blocks needs to be closed with the corresponding closing bracket".to_owned(),
                    current_span.clone(),
                )), // where it opened
                filename: self.filename.clone(),
                note: Some(format!(
                    "Consider closing the current block with the respective bracket"
                )),
                label: Some(format!("block starts here")),
                hint: Some((
                    format!("hint: consider adding }} here"),
                    current_span.clone().start + 1..current_span.clone().end + 1,
                )),
            });
        }

        self.expect(Token::RBrace)?;
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
                message: "Unexpected EOF in block".into(),
                span: 0..0,
                filename: self.filename.clone(),
                secondary_span: None,
                note: None,
                label: None,
                hint: None,
            }),
        }
    }

    fn parse_let_stmt(&mut self) -> ParseResult<Stmt> {
        let let_tok = self.next().unwrap();
        let name_tok = self.next().ok_or(ParseError {
            message: "Expected identifier after let".into(),
            span: let_tok.span.clone(),
            filename: self.filename.clone(),
            note: None,
            secondary_span: None,
            label: None,
            hint: None,
        })?;

        let name = match &name_tok.token {
            Token::Ident(s) => s.clone(),
            _ => {
                return Err(ParseError {
                    message: "Expected identifier after let".into(),
                    span: name_tok.span.clone(),
                    filename: self.filename.clone(),
                    note: None,
                    secondary_span: None,
                    label: None,
                    hint: None,
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
            message: "expected expression".to_string(),
            span: 0..0,
            filename: self.filename.clone(),
            note: None,
            secondary_span: None,
            label: None,
            hint: None,
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
            _ => Err(ParseError {
                message: format!("unexpected token in expression: {:?}", tok.token),
                span: tok.span.clone(),
                filename: self.filename.clone(),
                note: None,
                secondary_span: None,
                label: None,
                hint: None,
            }),
        }
    }

    fn check(&self, kind: Token) -> bool {
        matches!(self.tokens.get(self.pos).map(|t| &t.token), Some(k) if *k == kind)
    }

    fn expect_ident(&mut self) -> ParseResult<String> {
        let tok = self.next().ok_or(ParseError {
            message: "expected identifier".to_string(),
            span: 0..0,
            filename: self.filename.clone(),
            note: None,
            secondary_span: None,
            label: None,
            hint: None,
        })?;

        match tok.token {
            Token::Ident(name) => Ok(name),
            _ => Err(ParseError {
                message: format!("expected identifier, found {:?}", tok.token),
                span: tok.span.clone(),
                filename: self.filename.clone(),
                note: None,
                secondary_span: None,
                label: None,
                hint: None,
            }),
        }
    }
}

use ariadne::{Color, Label, Report, ReportKind, Source};

pub fn report_parse_error(source: &str, error: &ParseError) {
    let mut report = Report::build(ReportKind::Error, (&error.filename, error.span.clone()))
        .with_message(&error.message);

    if let Some(label) = error.label.clone() {
        report = report.with_label(
            Label::new((&error.filename, error.span.clone()))
                .with_message(label)
                .with_color(Color::Red),
        );
    } else {
        report = report.with_label(
            Label::new((&error.filename, error.span.clone()))
                .with_message("error occurs here")
                .with_color(Color::Red),
        );
    }

    if let Some(note) = error.note.clone() {
        report = report.with_note(note);
    };

    if let Some(sec_span) = error.secondary_span.clone() {
        let span = sec_span.1;
        let message = sec_span.0;
        report = report.with_label(
            Label::new((&error.filename, span.clone()))
                .with_message(message)
                .with_color(Color::Blue),
        );
    }

    if let Some((help_msg, help_span)) = &error.hint {
        report = report.with_label(
            Label::new((&error.filename, help_span.start + 1..help_span.end + 1))
                .with_message(help_msg)
                .with_color(Color::Cyan),
        );
    }

    report
        .finish()
        .print((&error.filename, Source::from(source)))
        .unwrap();
}

fn insertion_span_for_next_line(source: &str) -> Range<usize> {
    // find the last newline in source
    if let Some(pos) = source.rfind('\n') {
        let start = pos + 1;
        start..start // zero-width span at start of next line
    } else {
        0..0
    }
}
