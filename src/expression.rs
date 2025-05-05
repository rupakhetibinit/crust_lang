use core::fmt;

use crate::{error::Error, token::Token};

// use crate::{error::Error, token::Token};

#[derive(Debug, Clone)]
pub enum Expr {
    Binary {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },
    Literal {
        value: Object,
    },
    Unary {
        operator: Token,
        right: Box<Expr>,
    },
    Grouping {
        expr: Box<Expr>,
    },
}

#[derive(Debug, Clone)]
pub enum Object {
    Boolean(bool),
    Null,
    Number(f64),
    String(String),
}

impl Object {
    pub fn equals(&self, other: &Object) -> bool {
        match (self, other) {
            (Object::Null, Object::Null) => false,
            (_, Object::Null) => false,
            (Object::Null, _) => false,
            (Object::Boolean(left), Object::Boolean(right)) => left == right,
            (Object::Number(left), Object::Number(right)) => left == right,
            (Object::String(left), Object::String(right)) => left.eq(right),
            _ => false,
        }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Boolean(b) => write!(f, "{}", b),
            Object::Null => write!(f, "null"),
            Object::Number(n) => write!(f, "{}", n),
            Object::String(s) => write!(f, "{}", s),
        }
    }
}

pub struct AstPrinter;

impl AstPrinter {
    pub fn print(&self, expr: &Expr) -> Result<String, Error> {
        match expr {
            Expr::Binary {
                left,
                operator,
                right,
            } => self.parenthesize(&operator.lexeme, &[left, right]),
            Expr::Literal { value } => Ok(value.to_string()),
            Expr::Unary { operator, right } => self.parenthesize(&operator.lexeme, &[right]),
            Expr::Grouping { expr } => self.parenthesize("group", &[expr]),
        }
    }

    fn parenthesize(&self, name: &str, exprs: &[&Expr]) -> Result<String, Error> {
        let mut result = String::with_capacity(32);
        result.push('(');
        result.push_str(name);

        for expr in exprs {
            result.push(' ');
            result.push_str(&self.print(expr)?);
        }

        result.push(')');
        Ok(result)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::{Token, TokenType};
    #[test]
    fn test_printer() {
        let expression = Expr::Binary {
            left: Box::new(Expr::Unary {
                operator: Token::new(
                    TokenType::Minus,
                    "-".to_string(),
                    Some("-".to_string()),
                    1,
                    (1, 1),
                ),
                right: Box::new(Expr::Literal {
                    value: Object::Number(123f64),
                }),
            }),
            operator: Token::new(
                TokenType::Star,
                "*".to_string(),
                Some("*".to_string()),
                1,
                (1, 1),
            ),
            right: Box::new(Expr::Grouping {
                expr: Box::new(Expr::Literal {
                    value: Object::Number(45.67),
                }),
            }),
        };

        let printer = AstPrinter;
        assert_eq!(
            printer.print(&expression).unwrap(),
            format!("(* (- 123) (group 45.67))")
        );
    }
}
