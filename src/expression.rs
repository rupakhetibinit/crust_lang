use core::fmt;

use crate::{error::Error, token::Token};

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

pub trait Visitor<R> {
    fn visit_binary_expr(&self, left: &Expr, operator: &Token, right: &Expr) -> Result<R, Error>;
    fn visit_grouping_expr(&self, expression: &Expr) -> Result<R, Error>;
    fn visit_literal_expr(&self, value: &Object) -> Result<R, Error>;
    fn visit_unary_expr(&self, operator: &Token, right: &Expr) -> Result<R, Error>;
}

impl Expr {
    pub fn accept<R>(&self, visitor: &dyn Visitor<R>) -> Result<R, Error> {
        match self {
            Expr::Binary {
                left,
                operator,
                right,
            } => visitor.visit_binary_expr(left, operator, right),
            Expr::Literal { value } => visitor.visit_literal_expr(value),
            Expr::Unary { operator, right } => visitor.visit_unary_expr(operator, right),
            Expr::Grouping { expr } => visitor.visit_grouping_expr(expr),
        }
    }
}

pub struct AstPrinter;
impl AstPrinter {
    pub fn print(&self, expr: Expr) -> Result<String, Error> {
        expr.accept(self)
    }
    fn parenthesize(&self, name: String, exprs: Vec<&Expr>) -> Result<String, Error> {
        let mut r = String::new();
        r.push('(');
        r.push_str(&name);
        for e in &exprs {
            r.push(' ');
            r.push_str(&e.accept(self)?);
        }
        r.push(')');
        Ok(r)
    }
}

impl Visitor<String> for AstPrinter {
    fn visit_binary_expr(
        &self,
        left: &Expr,
        operator: &Token,
        right: &Expr,
    ) -> Result<String, Error> {
        self.parenthesize(operator.lexeme.clone(), vec![left, right])
    }
    fn visit_grouping_expr(&self, expr: &Expr) -> Result<String, Error> {
        self.parenthesize("group".to_string(), vec![expr])
    }
    fn visit_literal_expr(&self, value: &Object) -> Result<String, Error> {
        Ok(value.to_string())
    }
    fn visit_unary_expr(&self, operator: &Token, right: &Expr) -> Result<String, Error> {
        self.parenthesize(operator.lexeme.clone(), vec![right])
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
                operator: Token::new(TokenType::Minus, "-".to_string(), Some("-".to_string()), 1),
                right: Box::new(Expr::Literal {
                    value: Object::Number(123f64),
                }),
            }),
            operator: Token::new(TokenType::Star, "*".to_string(), Some("*".to_string()), 1),
            right: Box::new(Expr::Grouping {
                expr: Box::new(Expr::Literal {
                    value: Object::Number(45.67),
                }),
            }),
        };

        let printer = AstPrinter;
        assert_eq!(
            printer.print(expression).unwrap(),
            format!("(* (- 123) (group 45.67))")
        );
    }
}
