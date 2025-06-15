use ast::{
    LiteralValue, Parameter, TypeAnnotation, UntypedAstArena, UntypedAstNode, UntypedAstNodeId,
};
use lexer::{SpannedToken, Token};
use thiserror::Error;

pub struct Parser<'p> {
    tokens: &'p [SpannedToken<'p>],
    pos: usize,
    arena: UntypedAstArena<'p>,
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

    pub fn parse(&mut self) -> Result<(UntypedAstNodeId, UntypedAstArena<'p>), LocatedParserError> {
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
        let result = match self.current_token() {
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
            _ => self.parse_expression_inner(),
        };

        result.map_err(|e| self.with_location(e))
    }

    fn parse_expression_inner(&mut self) -> Result<UntypedAstNodeId, ParserError> {
        let token = self.consume()?;

        let node = match &token.token {
            Token::Int(i) => UntypedAstNode::Literal(LiteralValue::Int(*i)),
            Token::Float(f) => UntypedAstNode::Literal(LiteralValue::Float(*f)),
            _ => {
                return Err(ParserError::InvalidExpression {
                    token: token_to_string(&token.token),
                });
            }
        };

        Ok(self.arena.alloc(node))
    }

    fn parse_let_statement_inner(&mut self) -> Result<usize, ParserError> {
        self.expect_token(Token::Let)?;

        let identifier = self.expect_identifier()?;

        let ty = if self.match_and_consume(Token::Colon) {
            let ty = self.expect_identifier()?;
            Some(TypeAnnotation { ty })
        } else {
            None
        };

        if !self.match_and_consume(Token::Equal) {
            let found = self
                .current_token()
                .map(|t| token_to_string(&t.token))
                .unwrap_or_else(|| "end of input".to_string());
            return Err(ParserError::ExpectedAssignment { found });
        }

        let value_expr = self.parse_expression_inner()?;

        let stmt = UntypedAstNode::LetStatement {
            identifier,
            ty,
            value: value_expr,
        };

        Ok(self.arena.alloc(stmt))
    }

    fn parse_return_statement_inner(&mut self) -> Result<usize, ParserError> {
        self.expect_token(Token::Return)?;

        let expr = self.parse_expression_inner()?;

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
            name: fn_name,
            parameters: fn_parameters,
            return_type: TypeAnnotation { ty: fn_return_type },
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
            Some(token) => Err(ParserError::UnexpectedToken {
                expected: token_to_string(&expected),
                found: token_to_string(&token.token),
                span: Some(token.span.clone()),
            }),
            None => Err(ParserError::UnexpectedEndOfInput {
                expected: token_to_string(&expected),
            }),
        }
    }

    fn expect_identifier(&mut self) -> Result<&'p str, ParserError> {
        let token = self.consume()?;
        match token {
            SpannedToken {
                token: Token::Ident(ident),
                ..
            } => Ok(ident),
            _ => Err(ParserError::ExpectedIdentifier {
                found: token_to_string(&token.token),
            }),
        }
    }

    fn expect_type_annotation(&mut self) -> Result<&'p str, ParserError> {
        let token = self.consume()?;

        match token {
            SpannedToken {
                token: Token::Ident(ident),
                ..
            } => Ok(ident),
            _ => Err(ParserError::ExpectedTypeAnnotation {
                found: token_to_string(&token.token),
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

    fn parse_parameter_list(&mut self) -> Result<Vec<Parameter<'p>>, ParserError> {
        let mut parameters = Vec::new();

        if self.check_token(Token::RParen) {
            return Ok(parameters);
        }

        loop {
            let name = self.expect_identifier()?;
            self.expect_token(Token::Colon)?;
            let type_name = self.expect_type_annotation()?;

            parameters.push(Parameter {
                name,
                ty: TypeAnnotation { ty: type_name },
            });

            if self.match_and_consume(Token::Comma) {
                continue;
            } else if self.check_token(Token::RParen) {
                break;
            } else {
                let found = self
                    .current_token()
                    .map(|t| token_to_string(&t.token))
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
            _ => self.parse_expression_inner(),
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
}
fn extract_span_from_error(error: &ParserError) -> Option<std::ops::Range<usize>> {
    match error {
        ParserError::UnexpectedToken { span, .. } => span.clone(),
        _ => None,
    }
}

fn token_to_string(token: &Token<'_>) -> String {
    match token {
        Token::Fn => "fn".to_string(),
        Token::Let => "let".to_string(),
        Token::Return => "return".to_string(),
        Token::LParen => "(".to_string(),
        Token::RParen => ")".to_string(),
        Token::LBrace => "{".to_string(),
        Token::RBrace => "}".to_string(),
        Token::Colon => ":".to_string(),
        Token::Equal => "=".to_string(),
        Token::Comma => ",".to_string(),
        Token::Arrow => "->".to_string(),
        Token::Semicolon => ";".to_string(),
        Token::Ident(name) => format!("identifier '{}'", name),
        Token::Int(i) => format!("integer {}", i),
        Token::Float(f) => format!("float {}", f),
        Token::Plus => "+".to_string(),
        Token::Minus => "-".to_string(),
        Token::Star => "*".to_string(),
        Token::Slash => "/".to_string(),
        Token::LineComment(_) => "comment".to_string(),
        Token::Modulo => "%".to_string(),
        Token::StringLiteral(s) => format!("string \"{}\"", s),
        Token::Const => "const".to_string(),
        Token::LeftAngleBracket => "<".to_string(),
        Token::RightAngleBracket => ">".to_string(),
        Token::EqualEqual => "==".to_string(),
        Token::GreaterEqual => ">=".to_string(),
        Token::LesserEqual => "<=".to_string(),
        Token::BitAnd => "&".to_string(),
        Token::BitOr => "|".to_string(),
        Token::Or => "||".to_string(),
        Token::And => "&&".to_string(),
        Token::QuestionMark => "?".to_string(),
        Token::DoubleColon => "::".to_string(),
        Token::Not => "!".to_string(),
        Token::NotEqual => "!=".to_string(),
        Token::Dot => ".".to_string(),
    }
}

#[derive(Error, Debug)]
pub enum ParserError {
    #[error("Unexpected token: expected {expected}, found {found}")]
    UnexpectedToken {
        expected: String,
        found: String,
        span: Option<std::ops::Range<usize>>,
    },

    #[error("Expected type annotations found {found}")]
    ExpectedTypeAnnotation { found: String },

    #[error("Expected identifier, found {found}")]
    ExpectedIdentifier { found: String },

    #[error("Expected '{expected}', found '{found}'")]
    ExpectedToken { expected: String, found: String },

    #[error("Expected opening parenthesis '(', found {found}")]
    ExpectedOpeningParen { found: String },

    #[error("Expected closing parenthesis ')', found {found}")]
    ExpectedClosingParen { found: String },

    #[error("Expected colon ':', found {found}")]
    ExpectedColon { found: String },

    #[error("Expected comma ',' or closing parenthesis ')', found {found}")]
    ExpectedCommaOrClosingParen { found: String },

    #[error("Expected opening brace '{{', found {found}")]
    ExpectedOpeningBrace { found: String },

    #[error("Expected closing brace '}}', found {found}")]
    ExpectedClosingBrace { found: String },

    #[error("Expected assignment operator '=', found {found}")]
    ExpectedAssignment { found: String },

    #[error("Unexpected end of input, expected {expected}")]
    UnexpectedEndOfInput { expected: String },

    #[error("Invalid expression starting with {token}")]
    InvalidExpression { token: String },

    #[error("Empty parameter list not allowed")]
    EmptyParameterList,
}

#[derive(Error, Debug)]
#[error("Parse error at line {line}, column {column}: {source}")]
pub struct LocatedParserError {
    pub line: usize,
    pub column: usize,
    pub span: Option<std::ops::Range<usize>>,
    #[source]
    pub source: ParserError,
}

impl LocatedParserError {
    pub fn new(line: usize, column: usize, source: ParserError) -> Self {
        Self {
            line,
            column,
            span: None,
            source,
        }
    }

    pub fn with_span(
        line: usize,
        column: usize,
        span: std::ops::Range<usize>,
        source: ParserError,
    ) -> Self {
        Self {
            line,
            column,
            span: Some(span),
            source,
        }
    }
}

impl LocatedParserError {
    pub fn display_with_source(&self, source: &str, filename: Option<&str>) -> String {
        let mut output = String::new();

        if let Some(filename) = filename {
            output.push_str(&format!(
                "{}:{}:{}: ",
                filename,
                self.line + 1,
                self.column + 1
            ));
        }

        output.push_str(&format!("error: {}\n", self.source));

        let lines: Vec<&str> = source.lines().collect();

        if self.line < lines.len() {
            output.push_str(&format!("    |\n"));
            output.push_str(&format!("{:3} | {}\n", self.line + 1, lines[self.line]));
            output.push_str(&format!("    | "));

            if let Some(span) = &self.span {
                let mut line_start = 0;
                let mut current_line = 0;

                for (i, ch) in source.char_indices() {
                    if current_line == self.line {
                        line_start = i;
                        break;
                    }
                    if ch == '\n' {
                        current_line += 1;
                    }
                }

                let span_start_in_line = span.start.saturating_sub(line_start);
                let span_end_in_line =
                    (span.end.saturating_sub(line_start)).min(lines[self.line].len());

                for _ in 0..span_start_in_line {
                    output.push(' ');
                }

                let span_len = span_end_in_line.saturating_sub(span_start_in_line).max(1);
                for _ in 0..span_len {
                    output.push('^');
                }
            } else {
                for _ in 0..self.column {
                    output.push(' ');
                }
                output.push('^');
            }
            output.push('\n');
        }

        output
    }
}

pub fn print_ast(root_id: UntypedAstNodeId, arena: &UntypedAstArena<'_>) {
    fn print_node(node_id: UntypedAstNodeId, arena: &UntypedAstArena<'_>, indent: usize) {
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
                }
            }
            UntypedAstNode::ReturnStatement { value } => {
                println!("{}ReturnStatement", indent_str);
                print_node(*value, arena, indent + 2);
            }
            other => {
                println!("{}Other node: {:?}", indent_str, other);
            }
        }
    }

    print_node(root_id, arena, 0);
}

#[derive(Debug, Clone)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}
