use crate::lexer::Token;

type ParserError = String;

#[derive(Debug)]
pub enum AstNode {
    FnDecl {
        name: String,
        params: Vec<Params>,
        body: Vec<AstNode>,
        return_type: Option<Type>,
    },
    MainFunction {
        body: Vec<AstNode>,
    },
    LetBinding {
        var_name: String,
        var_type: Type,
        value: Box<AstNode>,
    },
    IfExpr {
        condition: Box<AstNode>,
        then_branch: Vec<AstNode>,
        else_branch: Option<Vec<AstNode>>,
    },
    BinaryExpr {
        left: Box<AstNode>,
        op: BinOp,
        right: Box<AstNode>,
    },
    Block {
        body: Vec<AstNode>,
    },
    Closure {
        params: Vec<Params>,
        body: Vec<AstNode>,
    },
    ReturnStatement {
        value: Box<AstNode>,
    },
}

#[derive(Debug)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,
}

#[derive(Debug)]
pub enum Type {
    Int32,
    Int64,
    Float32,
    Float64,
    Closure {
        params: Vec<Type>,
        return_type: Box<Type>,
    },
    UInt64,
    Unit,
}

#[derive(Debug)]
pub struct Params {
    name: String,
    ty: Type,
}

pub struct Parser<I: Iterator<Item = Token>> {
    tokens: I,
    peeked: Option<Token>,
}

impl<I: Iterator<Item = Token>> Parser<I> {
    pub fn new(tokens: I) -> Self {
        Parser {
            tokens,
            peeked: None,
        }
    }

    pub fn peek(&mut self) -> Option<&Token> {
        if self.peeked.is_none() {
            self.peeked = self.tokens.next()
        }
        self.peeked.as_ref()
    }

    pub fn next(&mut self) -> Option<Token> {
        self.peeked.take().or_else(|| self.tokens.next())
    }

    fn expect(&mut self, expected: Token) -> Result<Token, ParserError> {
        match self.next() {
            Some(t) if t == expected => Ok(t),
            Some(t) => Err(format!("Expected {:?}, got {:?}", expected, t)),
            None => Err(format!("Expected {:?}, found EOF", expected)),
        }
    }

    fn expect_ident(&mut self) -> Result<String, ParserError> {
        match self.next() {
            Some(Token::Identifier(name)) => Ok(name),
            Some(tok) => Err(format!("Expected identifier, got {:?}", tok)),
            None => Err("Expected identifier, got EOF".into()),
        }
    }

    pub fn parse_program(&mut self) -> Result<Vec<AstNode>, ParserError> {
        let nodes: Result<Vec<_>, _> = std::iter::from_fn(|| {
            if self.peek().is_some() {
                Some(self.parse_stmt())
            } else {
                None
            }
        })
        .collect();
        nodes
    }

    pub fn parse_stmt(&mut self) -> Result<AstNode, ParserError> {
        match self.next().expect("Token to be there") {
            Token::Let => self.parse_let_stmt(),
            Token::Return => self.parse_return_stmt(),
            Token::Fn => self.parse_fn_decl(),
            _ => self.parse_expression_stmt(),
        }
    }

    pub fn parse_fn_decl(&mut self) -> Result<AstNode, ParserError> {
        let name = self.expect_ident()?;
        let params = self.parse_fn_params()?;
        let return_type = self.parse_return_type()?;
        self.expect(Token::LBrace)?;
        let body = self.parse_block_stmt()?;
        self.expect(Token::RBrace)?;
        Ok(AstNode::FnDecl {
            name,
            params,
            body,
            return_type,
        })
    }

    pub fn parse_return_type(&mut self) -> Result<Option<Type>, ParserError> {
        self.expect(Token::TypeArrow)?;
        let ty = self.parse_type()?;
        Ok(Some(ty))
    }

    pub fn parse_type(&mut self) -> Result<Type, ParserError> {
        let ty = self.expect_type()?;
        Ok(ty)
    }

    pub fn parse_fn_params(&mut self) -> Result<Vec<Params>, ParserError> {
        self.expect(Token::LeftParen)?;
        let mut params = Vec::new();
        if self.peek().expect("Token to be here") != &Token::RightParen {
            let ident = self.expect_ident()?;
            let ty = self.parse_params_type()?;
            params.push(Params {
                name: ident,
                ty: ty,
            });
            while self.peek().expect("Token to be here") == &Token::Comma {
                let ident = self.expect_ident()?;
                let ty = self.parse_params_type()?;
                params.push(Params {
                    name: ident,
                    ty: ty,
                });
            }
        }
        self.expect(Token::RightParen)?;
        Ok(params)
    }

    pub fn parse_params_type(&mut self) -> Result<Type, ParserError> {
        self.expect(Token::Colon)?;
        let ty = self.expect_type()?;
        Ok(ty)
    }

    pub fn parse_return_stmt(&mut self) -> Result<AstNode, ParserError> {
        self.expect(Token::Return)?;
        let expr = Box::new(self.parse_expression_stmt()?);
        Ok(AstNode::ReturnStatement { value: expr })
    }

    pub fn parse_let_stmt(&mut self) -> Result<AstNode, ParserError> {
        let var_name = self.expect_ident()?;
        self.expect(Token::Colon)?;
        let var_type = self.expect_type()?;
        self.expect(Token::Equal)?;
        let expr = Box::new(self.parse_expression_stmt()?);
        Ok(AstNode::LetBinding {
            var_name,
            var_type,
            value: expr,
        })
    }

    pub fn parse_expression_stmt(&mut self) -> Result<AstNode, ParserError> {
        match self.next().expect("Tokent to be here") {
            Token::Pipe => self.parse_closure_function(),
            Token::Let => self.parse_let_stmt(),
            Token::If => self.parse_if_stmt(),
            x => Err(format!("Expected expression found {:?}", x)),
        }
    }

    pub fn parse_if_stmt(&mut self) -> Result<AstNode, ParserError> {
        self.expect(Token::LeftParen)?;
        let condition = Box::new(self.parse_expression_stmt()?);
        self.expect(Token::RightParen)?;

        let then_branch = self.parse_block_stmt()?;

        let else_branch = if self.peek().expect("Token to be here") == &Token::Else {
            self.next().expect("Token to be here");
            Some(self.parse_block_stmt()?)
        } else {
            None
        };

        Ok(AstNode::IfExpr {
            condition,
            then_branch,
            else_branch,
        })
    }

    pub fn parse_closure_function(&mut self) -> Result<AstNode, ParserError> {
        let params = self.parse_closure_fn_params()?;
        self.expect(Token::LBrace)?;
        let body = self.parse_block_stmt()?;
        self.expect(Token::RBrace)?;

        Ok(AstNode::Closure { params, body })
    }

    pub fn parse_block_stmt(&mut self) -> Result<Vec<AstNode>, ParserError> {
        let mut stmts = Vec::new();
        while self.peek().expect("Token to be here") != &Token::RBrace {
            stmts.push(self.parse_expression_stmt()?);
        }
        Ok(stmts)
    }

    pub fn expect_type(&mut self) -> Result<Type, ParserError> {
        let ty = self.next().expect("Type to be in there");
        match ty {
            Token::Fn => self.parse_closure_type(),
            Token::I64 => Ok(Type::Int64),
            Token::U64 => Ok(Type::UInt64),
            _ => Err("That's not a type".to_string()),
        }
    }

    pub fn parse_closure_type(&mut self) -> Result<Type, ParserError> {
        self.expect(Token::LeftParen)?;
        let params = self.parse_closure_param_list()?;
        self.expect(Token::TypeArrow)?;
        let return_type = self.expect_type()?;
        Ok(Type::Closure {
            params,
            return_type: Box::new(return_type),
        })
    }

    pub fn parse_closure_fn_params(&mut self) -> Result<Vec<Params>, ParserError> {
        let mut params = Vec::new();
        loop {
            let ident = self.expect_ident()?;
            self.expect(Token::Colon)?;
            let ty = self.expect_type()?;
            params.push(Params { name: ident, ty });
            match self.next().expect("Token to be there") {
                Token::Comma => continue,
                Token::Pipe => break,
                x => return Err(format!("Expected type, got {:?}", x)),
            }
        }
        Ok(params)
    }

    pub fn parse_closure_param_list(&mut self) -> Result<Vec<Type>, ParserError> {
        let mut params = Vec::new();
        loop {
            match self.next().expect("Token to be there") {
                Token::RightParen => break,
                Token::Comma => continue,
                Token::I64 => params.push(Type::Int64),
                Token::U64 => params.push(Type::UInt64),
                x => return Err(format!("Expected type, got {:?}", x)),
            }
        }
        Ok(params)
    }
}
