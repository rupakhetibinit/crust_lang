use crate::expression::{Expr, Visitor};

enum Object {
    Boolean(bool),
    Null,
    Number(f64),
    String(String),
}

pub struct Interpreter;

impl Interpreter {
    fn evaluate(&self, expression: &Expr) -> String {
        expression.accept(self)
    }
}

pub fn is_truthy(expression: String) -> bool {
    if expression.is_empty() {
        return false;
    }

    match expression.parse::<bool>() {
        Ok(x) => x,
        Err(_) => false,
    }
}

impl Visitor<String> for Interpreter {
    fn visit_binary_expr(
        &self,
        left: &crate::expression::Expr,
        operator: &crate::token::Token,
        right: &crate::expression::Expr,
    ) -> String {
        let left = self.evaluate(left);
        let right = self.evaluate(right);

        match operator.token_type {
            crate::token::TokenType::Minus => "".to_string(),
            crate::token::TokenType::Plus => "".to_string(),
            crate::token::TokenType::Slash => {
                (left.parse::<f64>().unwrap() / right.parse::<f64>().unwrap()).to_string()
            }
            crate::token::TokenType::Star => {
                (left.parse::<f64>().unwrap() + right.parse::<f64>().unwrap()).to_string()
            }
            _ => "".to_string(),
        }
    }

    fn visit_grouping_expr(&self, expression: &crate::expression::Expr) -> String {
        self.evaluate(expression)
    }

    fn visit_literal_expr(&self, value: String) -> String {
        return value;
    }

    fn visit_unary_expr(
        &self,
        operator: &crate::token::Token,
        right: &crate::expression::Expr,
    ) -> String {
        let right = self.evaluate(right);

        match operator.token_type {
            crate::token::TokenType::Bang => (!is_truthy(right)).to_string(),
            crate::token::TokenType::Minus => (-right.parse::<f64>().unwrap()).to_string(),
            _ => "".to_string(),
        }
    }
}
