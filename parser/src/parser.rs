use crate::{LocatedParserError, ParserError, extract_span_from_error};
use ast::{
    BinOp, LiteralValue, Parameter, TypeAnnotation, UnaryOp, UntypedAstArena, UntypedAstNode,
    UntypedAstNodeId,
};
use lexer::{SpannedToken, Token};

#[derive(Debug, Clone)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

pub struct Parser<'p> {
    tokens: &'p [SpannedToken<'p>],
    pos: usize,
    arena: UntypedAstArena,
    source_text: &'p str,
}

impl<'p> Parser<'p> {
    pub fn new(tokens: &'p [SpannedToken<'p>], source_text: &'p str) -> Self {
        Self {
            tokens,
            pos: 0,
            arena: UntypedAstArena::default(),
            source_text,
        }
    }

    pub fn parse(&mut self) -> Result<(UntypedAstNodeId, UntypedAstArena), LocatedParserError> {
        let root_id = self.parse_program()?;
        Ok((root_id, std::mem::take(&mut self.arena)))
    }

    fn parse_program(&mut self) -> Result<UntypedAstNodeId, LocatedParserError> {
        let mut statements = Vec::new();

        while !self.is_at_end() {
            statements.push(self.parse_statement()?);
        }

        Ok(self.arena.alloc(UntypedAstNode::Program(statements)))
    }

    fn parse_statement(&mut self) -> Result<UntypedAstNodeId, LocatedParserError> {
        let token_type = {
            let current = self.current_token();
            current.map(|st| &st.token) // Get a reference to the token type
        };

        let result = match token_type {
            Some(Token::Fn) => self.parse_fn_definition_inner(),
            Some(Token::Let) => self.parse_let_statement_inner(),
            Some(Token::Return) => self.parse_return_statement_inner(),
            Some(Token::LineComment(_)) | Some(Token::MultiLineComment(_)) => self.parse_comment(),
            Some(Token::For) => self.parse_for_loop_inner(),
            _ => self.parse_primary(),
        };

        result.map_err(|e| self.with_location(e))
    }

    fn parse_primary(&mut self) -> Result<UntypedAstNodeId, ParserError> {
        let span_token = self.consume()?;
        let token = span_token.token.clone();

        let node = match &token {
            Token::Ident(name) => {
                // Check if the next token is a left parenthesis, indicating a function call
                if self.match_and_consume(Token::LParen) {
                    // It's a function call
                    let mut args = Vec::new();

                    if !self.check_token(Token::RParen) {
                        loop {
                            args.push(self.parse_expr_inner(0)?);

                            if self.match_and_consume(Token::Comma) {
                                continue;
                            } else if self.check_token(Token::RParen) {
                                break;
                            } else {
                                let found = self
                                    .current_token()
                                    .map(|t| t.token.to_string())
                                    .unwrap_or_else(|| "end of input".to_string());
                                return Err(ParserError::ExpectedCommaOrClosingParen { found });
                            }
                        }
                    }

                    self.expect_token(Token::RParen)?;

                    UntypedAstNode::FunctionCall {
                        callee: name.to_string(),
                        arguments: args,
                    }
                } else {
                    UntypedAstNode::Ident(name.to_string())
                }
            }
            Token::Int(i) => UntypedAstNode::Literal(LiteralValue::Int(*i)),
            Token::Float(f) => UntypedAstNode::Literal(LiteralValue::Float(*f)),
            Token::StringLiteral(raw_string) => {
                UntypedAstNode::Literal(LiteralValue::RawString(raw_string.to_string()))
            }
            Token::LParen => {
                let inner_expr = self.parse_expr_inner(0)?;
                self.expect_token(Token::RParen)?;
                return Ok(inner_expr);
            }

            _ => {
                return Err(ParserError::InvalidExpression {
                    token: span_token.token.to_string(),
                });
            }
        };

        let id = self.arena.alloc(node);

        return Ok(id);
    }

    fn parse_expr_inner(&mut self, min_prec: u8) -> Result<UntypedAstNodeId, ParserError> {
        let mut left = self.parse_unary()?;

        loop {
            let op_token = match self.current_token().cloned() {
                Some(
                    tok @ SpannedToken {
                        token:
                            Token::Plus
                            | Token::Minus
                            | Token::Star
                            | Token::Slash
                            | Token::Modulo
                            | Token::Or
                            | Token::And
                            | Token::NotEqual
                            | Token::EqualEqual
                            | Token::GreaterEqual
                            | Token::LesserEqual
                            | Token::BitAnd
                            | Token::BitOr,
                        ..
                    },
                ) => tok,
                _ => return Ok(left),
            };

            let (prec, bin_op) = match self.get_binop_prec_and_kind(&op_token.token) {
                Some((prec, bin_op)) if prec >= min_prec => (prec, bin_op),
                _ => break,
            };

            self.consume()?;

            let right = self.parse_expr_inner(prec + 1)?;

            let node = UntypedAstNode::BinaryExpression {
                left,
                op: bin_op,
                right,
            };

            left = self.arena.alloc(node);
        }

        Ok(left)
    }

    fn parse_let_statement_inner(&mut self) -> Result<UntypedAstNodeId, ParserError> {
        self.expect_token(Token::Let)?;

        let identifier = self.expect_identifier()?;

        let ty = if self.match_and_consume(Token::Colon) {
            let ty = self.expect_identifier()?;
            Some(TypeAnnotation { ty: ty.to_owned() })
        } else {
            None
        };

        if !self.match_and_consume(Token::Equal) {
            let found = self
                .current_token()
                .map(|t| t.token.to_string())
                .unwrap_or_else(|| "end of input".to_string());
            return Err(ParserError::ExpectedAssignment { found });
        }

        let value_expr = self.parse_expr_inner(0)?;

        let stmt = UntypedAstNode::LetStatement {
            identifier: identifier.to_owned(),
            ty,
            value: value_expr,
        };

        Ok(self.arena.alloc(stmt))
    }

    fn parse_return_statement_inner(&mut self) -> Result<usize, ParserError> {
        self.expect_token(Token::Return)?;

        let expr = self.parse_expr_inner(0)?;

        Ok(self
            .arena
            .alloc(UntypedAstNode::ReturnStatement { value: expr }))
    }

    fn parse_fn_definition_inner(&mut self) -> Result<usize, ParserError> {
        self.expect_token(Token::Fn)?;

        let fn_name = self.expect_identifier()?;

        self.expect_token(Token::LParen)?;

        let fn_parameters = self.parse_parameter_list()?;

        self.expect_token(Token::RParen)?;

        self.expect_token(Token::Arrow)?;

        let fn_return_type = self.expect_type_annotation()?;

        let fn_body = self.parse_block_inner()?;

        let function_node = UntypedAstNode::FunctionDefinition {
            name: fn_name.to_owned(),
            parameters: fn_parameters,
            return_type: TypeAnnotation {
                ty: fn_return_type.to_owned(),
            },
            body: fn_body,
        };

        Ok(self.arena.alloc(function_node))
    }

    fn prev_token(&self) -> Option<&SpannedToken<'p>> {
        self.tokens.get(self.pos - 1)
    }

    fn current_token(&self) -> Option<&SpannedToken<'p>> {
        self.tokens.get(self.pos)
    }

    fn consume(&mut self) -> Result<&SpannedToken<'p>, ParserError> {
        if self.pos < self.tokens.len() {
            let tok = &self.tokens[self.pos];
            self.pos += 1;
            Ok(tok)
        } else {
            Err(ParserError::UnexpectedEndOfInput {
                expected: "token".to_string(),
            })
        }
    }

    fn expect_token(&mut self, expected: Token) -> Result<(), ParserError> {
        match self.current_token() {
            Some(token)
                if std::mem::discriminant(&token.token) == std::mem::discriminant(&expected) =>
            {
                self.consume()?;
                Ok(())
            }
            Some(token) => {
                return Err(ParserError::UnexpectedToken {
                    expected: expected.to_string(),
                    found: token.token.to_string(),
                    span: Some(token.span.clone()),
                });
            }
            None => {
                return Err(ParserError::UnexpectedEndOfInput {
                    expected: expected.to_string(),
                });
            }
        }
    }

    fn expect_identifier(&mut self) -> Result<&'p str, ParserError> {
        let span_token = self.consume()?;
        match span_token {
            SpannedToken {
                token: Token::Ident(ident),
                ..
            } => Ok(ident),
            _ => Err(ParserError::ExpectedIdentifier {
                found: span_token.token.to_string(),
            }),
        }
    }

    fn expect_type_annotation(&mut self) -> Result<&'p str, ParserError> {
        let span_token = self.consume()?;

        match span_token {
            SpannedToken {
                token: Token::Ident(ident),
                ..
            } => Ok(ident),
            _ => Err(ParserError::ExpectedTypeAnnotation {
                found: span_token.token.to_string(),
            }),
        }
    }

    fn is_at_end(&self) -> bool {
        self.pos >= self.tokens.len()
    }

    fn check_token(&mut self, token_type: Token) -> bool {
        if let Some(current) = self.current_token() {
            std::mem::discriminant(&current.token) == std::mem::discriminant(&token_type)
        } else {
            false
        }
    }

    fn match_and_consume(&mut self, token_type: Token) -> bool {
        if self.check_token(token_type) {
            self.pos += 1;
            true
        } else {
            false
        }
    }

    fn parse_parameter_list(&mut self) -> Result<Vec<Parameter>, ParserError> {
        let mut parameters = Vec::new();

        if self.check_token(Token::RParen) {
            return Ok(parameters);
        }

        loop {
            let name = self.expect_identifier()?;
            self.expect_token(Token::Colon)?;
            let type_name = self.expect_type_annotation()?;

            parameters.push(Parameter {
                name: name.to_owned(),
                ty: TypeAnnotation {
                    ty: type_name.to_owned(),
                },
            });

            if self.match_and_consume(Token::Comma) {
                continue;
            } else if self.check_token(Token::RParen) {
                break;
            } else {
                let found = self
                    .current_token()
                    .map(|t| t.token.to_string())
                    .unwrap_or_else(|| "end of input".to_string());
                return Err(ParserError::ExpectedCommaOrClosingParen { found });
            }
        }

        Ok(parameters)
    }

    fn parse_block_inner(&mut self) -> Result<UntypedAstNodeId, ParserError> {
        self.expect_token(Token::LBrace)?;

        let mut statements = Vec::new();

        while !self.check_token(Token::RBrace) && !self.is_at_end() {
            statements.push(self.parse_statement_inner()?);

            self.match_and_consume(Token::Semicolon);
        }

        self.expect_token(Token::RBrace)?;

        let block_node = UntypedAstNode::Block(statements);

        Ok(self.arena.alloc(block_node))
    }

    fn parse_statement_inner(&mut self) -> Result<UntypedAstNodeId, ParserError> {
        match self.current_token() {
            Some(SpannedToken {
                token: Token::Fn, ..
            }) => self.parse_fn_definition_inner(),
            Some(SpannedToken {
                token: Token::Let, ..
            }) => self.parse_let_statement_inner(),
            Some(SpannedToken {
                token: Token::Return,
                ..
            }) => self.parse_return_statement_inner(),
            _ => self.parse_expr_inner(0),
        }
    }

    fn position_from_offset(&self, offset: usize) -> (usize, usize) {
        let mut line = 0;
        let mut column = 0;

        for (i, ch) in self.source_text.char_indices() {
            if i >= offset {
                break;
            }

            if ch == '\n' {
                line += 1;
                column = 0;
            } else {
                column += 1;
            }
        }

        (line, column)
    }

    fn with_location(&self, error: ParserError) -> LocatedParserError {
        if let Some(span) = extract_span_from_error(&error) {
            let (line, column) = self.position_from_offset(span.start);
            return LocatedParserError::with_span(line, column, span, error);
        }

        if let Some(token) = self.prev_token() {
            let (line, column) = self.position_from_offset(token.span.start);
            return LocatedParserError::with_span(line, column, token.span.clone(), error);
        }

        LocatedParserError::new(0, 0, error)
    }

    fn parse_comment(&mut self) -> Result<UntypedAstNodeId, ParserError> {
        let token_pos = self.pos;

        let token = self.consume()?;

        match &token.token {
            Token::LineComment(_) => {}
            Token::MultiLineComment(_) => {}
            _ => unreachable!(),
        };

        let comment_text = match &self.tokens[token_pos].token {
            Token::LineComment(text) => text,
            Token::MultiLineComment(text) => text,
            _ => unreachable!(),
        };

        let node = UntypedAstNode::Comment(comment_text.to_owned());
        let id = self.arena.alloc(node);
        Ok(id)
    }
    fn get_binop_prec_and_kind(&self, token: &Token<'p>) -> Option<(u8, BinOp)> {
        match token {
            Token::Plus => Some((10, BinOp::Add)),
            Token::Minus => Some((10, BinOp::Sub)),
            Token::Star => Some((20, BinOp::Multiply)),
            Token::Slash => Some((20, BinOp::Divide)),
            Token::LesserEqual => Some((5, BinOp::LesserEqual)),
            Token::GreaterEqual => Some((5, BinOp::GreaterEqual)),
            _ => None,
        }
    }

    fn parse_for_loop_inner(&mut self) -> Result<UntypedAstNodeId, ParserError> {
        self.expect_token(Token::For)?;
        // Parse let statement for initializer
        self.expect_token(Token::LParen)?;
        let initializer = self.parse_let_statement_inner()?;
        self.expect_token(Token::Semicolon)?;
        // Parse condition expression
        let expression = self.parse_expr_inner(0)?;
        self.expect_token(Token::Semicolon)?;
        // Parse increment expression
        let increment = self.parse_expr_inner(0)?;
        self.expect_token(Token::RParen)?;

        // Parse block of statements
        let block = self.parse_block_inner()?;

        let for_loop_node = UntypedAstNode::ForLoop {
            initializer,
            condition: expression,
            increment,
            block,
        };

        Ok(self.arena.alloc(for_loop_node))
    }

    fn parse_unary(&mut self) -> Result<UntypedAstNodeId, ParserError> {
        if let Some(token) = self.current_token() {
            match &token.token {
                Token::PlusPlus => {
                    self.consume()?;
                    let operand = self.parse_unary()?;
                    let node = UntypedAstNode::UnaryExpression {
                        op: UnaryOp::PreIncrement,
                        expression: operand,
                    };
                    return Ok(self.arena.alloc(node));
                }
                Token::MinusMinus => {
                    self.consume()?;
                    let operand = self.parse_unary()?;
                    let node = UntypedAstNode::UnaryExpression {
                        op: UnaryOp::PreDecrement,
                        expression: operand,
                    };
                    return Ok(self.arena.alloc(node));
                }
                Token::Not => {
                    self.consume()?;
                    let operand = self.parse_unary()?;
                    let node = UntypedAstNode::UnaryExpression {
                        op: UnaryOp::Not,
                        expression: operand,
                    };
                    return Ok(self.arena.alloc(node));
                }
                Token::Minus => {
                    self.consume()?;
                    let operand = self.parse_unary()?;
                    let node = UntypedAstNode::UnaryExpression {
                        op: UnaryOp::Negate,
                        expression: operand,
                    };
                    return Ok(self.arena.alloc(node));
                }
                _ => {}
            }
        }
        self.parse_postfix()
    }

    fn parse_postfix(&mut self) -> Result<usize, ParserError> {
        let mut expr = self.parse_primary()?;

        loop {
            match self.current_token() {
                Some(SpannedToken {
                    token: Token::PlusPlus,
                    ..
                }) => {
                    self.consume()?;
                    expr = self.arena.alloc(UntypedAstNode::UnaryExpression {
                        op: UnaryOp::PostIncrement,
                        expression: expr,
                    });
                }
                Some(SpannedToken {
                    token: Token::MinusMinus,
                    ..
                }) => {
                    self.consume()?;
                    expr = self.arena.alloc(UntypedAstNode::UnaryExpression {
                        op: UnaryOp::PostDecrement,
                        expression: expr,
                    });
                }
                _ => {
                    break;
                }
            }
        }

        Ok(expr)
    }
}

pub fn print_ast(root_id: UntypedAstNodeId, arena: &UntypedAstArena) {
    fn print_node(node_id: UntypedAstNodeId, arena: &UntypedAstArena, indent: usize) {
        let indent_str = "  ".repeat(indent);
        let node = arena.get(node_id);

        match node {
            UntypedAstNode::Program(statements) => {
                println!("{}Program", indent_str);
                for stmt_id in statements {
                    print_node(*stmt_id, arena, indent + 1);
                }
            }
            UntypedAstNode::LetStatement {
                identifier,
                ty,
                value,
            } => {
                println!("{}LetStatement", indent_str);
                println!("{}  Identifier: {}", indent_str, identifier);
                match ty {
                    Some(TypeAnnotation { ty }) => {
                        println!("{}  Type: {}", indent_str, ty);
                    }
                    None => {
                        println!("{}  Type: <inferred>", indent_str);
                    }
                }
                println!("{}  Value:", indent_str);
                print_node(*value, arena, indent + 2);
            }
            UntypedAstNode::Literal(x) => {
                println!("{}Literal Value", indent_str);
                match x {
                    LiteralValue::RawString(x) => println!("{} String {}", indent_str, x),
                    LiteralValue::Int(i) => println!("{} Int {}", indent_str, i),
                    LiteralValue::Float(f) => println!("{} Float {}", indent_str, f),
                    LiteralValue::Bool(b) => println!("{} Boolean {}", indent_str, b),
                }
            }
            UntypedAstNode::ReturnStatement { value } => {
                println!("{}ReturnStatement", indent_str);
                print_node(*value, arena, indent + 2);
            }
            UntypedAstNode::FunctionDefinition {
                name,
                parameters,
                return_type,
                body,
            } => {
                println!("{}Function Declaration", indent_str);
                println!("{}  Name: {}", indent_str, name);
                println!("{}  Parameters:", indent_str);
                for param in parameters {
                    println!("{}    {}: {}", indent_str, param.name, param.ty.ty);
                }
                println!("{}  Return Type: {}", indent_str, return_type.ty);
                println!("{}  Body:", indent_str);
                print_node(*body, arena, indent + 2);
            }
            UntypedAstNode::Block(statements) => {
                println!("{}Block", indent_str);
                if statements.is_empty() {
                    println!("{}  <empty>", indent_str);
                }
                for stmt_id in statements {
                    print_node(*stmt_id, arena, indent + 1);
                }
            }
            UntypedAstNode::BinaryExpression { left, op, right } => {
                println!("{}BinaryExpression", indent_str);
                println!("{}  Operator: {:?}", indent_str, op);
                println!("{}  Left:", indent_str);
                print_node(*left, arena, indent + 1);
                println!("{}  Right:", indent_str);
                print_node(*right, arena, indent + 1);
            }
            UntypedAstNode::Ident(name) => {
                println!("{}Identifier: {}", indent_str, name);
            }
            UntypedAstNode::Comment(comment) => {
                println!("{}Comment: {}", indent_str, comment);
            }
            UntypedAstNode::ForLoop {
                initializer,
                condition,
                increment,
                block,
            } => {
                println!("{}ForLoop", indent_str);
                println!("{}  Initializer:", indent_str);
                print_node(*initializer, arena, indent + 1);
                println!("{}  Condition:", indent_str);
                print_node(*condition, arena, indent + 1);
                println!("{}  Increment:", indent_str);
                print_node(*increment, arena, indent + 1);
                println!("{}  Block:", indent_str);
                print_node(*block, arena, indent + 1);
            }
            UntypedAstNode::UnaryExpression { op, expression } => {
                println!("{}UnaryExpression", indent_str);
                println!("{}  Operator: {:?}", indent_str, op);
                println!("{}  Expression:", indent_str);
                print_node(*expression, arena, indent + 1);
            }
            UntypedAstNode::FunctionCall { callee, arguments } => {
                println!("{}FunctionCall", indent_str);
                println!("{}  FunctinName:", indent_str);
                println!("{}    {}", indent_str, callee);
                if arguments.is_empty() {
                    println!("{}  Arguments: <none>", indent_str);
                } else {
                    println!("{}  Arguments:", indent_str);
                    for arg_id in arguments {
                        print_node(*arg_id, arena, indent + 1);
                    }
                }
            }
        }
    }

    print_node(root_id, arena, 0);
}
