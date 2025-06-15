use ast::{TypeAnnotation, UntypedAstArena, UntypedAstNode, UntypedAstNodeId};
use lexer::{SpannedToken, Token};

pub struct Parser<'p> {
    tokens: &'p [SpannedToken<'p>],
    pos: usize,
    arena: UntypedAstArena<'p>,
}

impl<'p> Parser<'p> {
    pub fn new(tokens: &'p [SpannedToken<'p>]) -> Self {
        Self {
            tokens,
            pos: 0,
            arena: UntypedAstArena::default(),
        }
    }

    pub fn parse(&mut self) -> Result<(UntypedAstNodeId, UntypedAstArena<'p>), ParserError> {
        let root_id = self.parse_program()?;
        Ok((root_id, std::mem::take(&mut self.arena)))
    }

    fn parse_program(&mut self) -> Result<UntypedAstNodeId, ParserError> {
        let mut statements = Vec::new();

        while self.peek().is_some() {
            let stmt = self.parse_statement()?;
            statements.push(stmt);
        }

        let program_node = UntypedAstNode::Program(statements);

        Ok(self.arena.alloc(program_node))
    }

    fn parse_statement(&mut self) -> Result<UntypedAstNodeId, ParserError> {
        match self.peek() {
            Some(SpannedToken {
                token: lexer::Token::Let,
                ..
            }) => self.parse_let_statement(),
            _ => self.parse_expression(),
        }
    }

    fn peek(&self) -> Option<&SpannedToken<'p>> {
        self.tokens.get(self.pos)
    }

    fn advance(&mut self) -> Option<&SpannedToken<'p>> {
        if self.pos < self.tokens.len() {
            let tok = &self.tokens[self.pos];
            self.pos += 1;
            Some(tok)
        } else {
            None
        }
    }

    fn expect_keyword(&mut self, keyword: &str) -> Result<(), ParserError> {
        if let Some(tok) = self.advance() {
            if let lexer::Token::Ident(ident) = tok.token {
                if ident == keyword {
                    return Ok(());
                }
            }
        }
        Err(ParserError::Error)
    }

    fn parse_expression(&self) -> Result<usize, ParserError> {
        todo!()
    }

    fn parse_let_statement(&mut self) -> Result<usize, ParserError> {
        self.advance();

        let identifier = match self.advance() {
            Some(SpannedToken {
                token: Token::Ident(name),
                ..
            }) => *name,
            _ => return Err(ParserError::UnexpectedToken),
        };

        let ty = if let Some(SpannedToken {
            token: Token::Colon,
            ..
        }) = self.peek()
        {
            self.advance();

            match self.advance() {
                Some(SpannedToken {
                    token: Token::Ident(type_name),
                    ..
                }) => Some(TypeAnnotation { ty: *type_name }),
                _ => return Err(ParserError::ExpectedIdentifier),
            }
        } else {
            None
        };

        match self.advance() {
            Some(SpannedToken {
                token: Token::Equal,
                ..
            }) => {}
            _ => return Err(ParserError::Error),
        };

        let value_expr = self.parse_expression()?;

        let stmt = UntypedAstNode::LetStatement {
            identifier,
            ty,
            value: value_expr,
        };

        Ok(self.arena.alloc(stmt))
    }
}

#[derive(Debug)]
pub enum ParserError {
    Error,
    UnexpectedToken,
    ExpectedIdentifier,
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
            other => {
                println!("{}Other node: {:?}", indent_str, other);
            }
        }
    }

    print_node(root_id, arena, 0);
}
