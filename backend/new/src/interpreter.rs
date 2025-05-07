use std::{collections::HashMap, string::ParseError};

use crate::{
    ast::{AstKind, AstNodeId},
    parser::Parser,
    token::Token,
};

pub struct Interpreter<'a> {
    pub parser: Parser<'a>,
    env_values: HashMap<String, String>,
}

impl<'a> Interpreter<'a> {
    pub fn new(parser: Parser<'a>) -> Self {
        Self {
            parser,
            env_values: HashMap::new(),
        }
    }

    pub fn evaluate(&mut self) -> Result<(), ParseError> {
        while self.parser.peek() != &Token::EOF {
            let statement = self.parser.parse_stmt();
            match statement {
                Ok(value) => {
                    let value = self.evaluate_statement(value);
                    println!("{}", value);
                }
                Err(err) => {
                    println!("Error {:?}", err);
                }
            }
        }
        Ok(())
    }

    fn evaluate_statement(&mut self, statement: AstNodeId) -> String {
        let stmt = self.parser.ast.get(statement).unwrap();
        match stmt.kind {
            AstKind::Number(x) => format!("{}", x),
            AstKind::Var(value) => {
                let get_if_its_variable = self.parser.interner.resolve(value).unwrap();
                let actual_value = self.env_values.get(get_if_its_variable).unwrap();
                format!("{}", actual_value)
            }
            AstKind::RawString(interned_string) => {
                let actual = self.parser.interner.resolve(interned_string).unwrap();
                format!("{}", actual)
            }
            AstKind::Add(left, right) => {
                let lhs = self.evaluate_statement(left).parse::<i64>().unwrap();
                let rhs = self.evaluate_statement(right).parse::<i64>().unwrap();
                format!("{}", lhs + rhs)
            }
            AstKind::Div(left, right) => {
                let lhs = self.evaluate_statement(left).parse::<i64>().unwrap();
                let rhs = self.evaluate_statement(right).parse::<i64>().unwrap();
                format!("{}", lhs / rhs)
            }
            AstKind::Mul(left, right) => {
                let lhs = self.evaluate_statement(left).parse::<i64>().unwrap();
                let rhs = self.evaluate_statement(right).parse::<i64>().unwrap();
                format!("{}", lhs * rhs)
            }
            AstKind::Sub(left, right) => {
                let lhs = self.evaluate_statement(left).parse::<i64>().unwrap();
                let rhs = self.evaluate_statement(right).parse::<i64>().unwrap();
                format!("{}", lhs - rhs)
            }
            AstKind::Assign(variable, expression) => {
                let val = self.parser.interner.resolve(variable).unwrap().to_owned();

                let value = self.evaluate_statement(expression);

                self.env_values.insert(val, value);
                "".to_owned()
            }
            AstKind::Pow(_base, _exp) => format!("In Progress"),
            AstKind::Print(ast_node_id) => {
                let value = self.evaluate_statement(ast_node_id);
                return value;
            }
        }
    }
}
