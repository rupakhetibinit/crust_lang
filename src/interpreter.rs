use crate::{
    error::Error,
    expression::{Expr, Object, Visitor},
    token::{Token, TokenType},
};

pub struct Interpreter;

impl Interpreter {
    pub fn evaluate(&self, expression: &Expr) -> Result<Object, Error> {
        expression.accept(self)
    }

    fn is_truthy(&self, object: &Object) -> bool {
        match object {
            Object::Null => false,
            Object::Boolean(b) => b.clone(),
            _ => true,
        }
    }

    fn is_equal(&self, l: Object, r: Object) -> bool {
        l.equals(&r)
    }

    fn number_operand_error(&self, operator: &Token) -> Result<Object, Error> {
        Err(Error::Runtime {
            token: operator.clone(),
            message: "Operand must be a number".to_string(),
        })
    }
}

impl Visitor<Object> for Interpreter {
    fn visit_binary_expr(
        &self,
        left: &Expr,
        operator: &Token,
        right: &Expr,
    ) -> Result<Object, Error> {
        let l = self.evaluate(left)?;
        let r = self.evaluate(right)?;

        match operator.token_type {
            TokenType::Greater => match (l, r) {
                (Object::Number(left_number), Object::Number(right_number)) => {
                    return Ok(Object::Boolean(left_number > right_number))
                }
                _ => self.number_operand_error(operator),
            },
            TokenType::Less => match (l, r) {
                (Object::Number(left_number), Object::Number(right_number)) => {
                    return Ok(Object::Boolean(left_number < right_number))
                }
                _ => self.number_operand_error(operator),
            },
            TokenType::LessEqual => match (l, r) {
                (Object::Number(left_number), Object::Number(right_number)) => {
                    return Ok(Object::Boolean(left_number <= right_number))
                }
                _ => self.number_operand_error(operator),
            },
            TokenType::GreaterEqual => match (l, r) {
                (Object::Number(left_number), Object::Number(right_number)) => {
                    return Ok(Object::Boolean(left_number >= right_number))
                }
                _ => self.number_operand_error(operator),
            },
            TokenType::Minus => match (l, r) {
                (Object::Number(left_number), Object::Number(right_number)) => {
                    return Ok(Object::Number(left_number - right_number))
                }
                _ => self.number_operand_error(operator),
            },
            TokenType::Star => match (l, r) {
                (Object::Number(left_number), Object::Number(right_number)) => {
                    return Ok(Object::Number(left_number * right_number))
                }
                _ => self.number_operand_error(operator),
            },
            TokenType::Slash => match (l, r) {
                (Object::Number(left_number), Object::Number(right_number)) => {
                    return Ok(Object::Number(left_number / right_number))
                }
                _ => self.number_operand_error(operator),
            },
            TokenType::Plus => match (l, r) {
                (Object::String(left_string), Object::String(right_string)) => {
                    return Ok(Object::String(format!("{}{}", left_string, right_string)));
                }
                (Object::Number(left_num), Object::Number(right_num)) => {
                    return Ok(Object::Number(left_num + right_num));
                }
                _ => Err(Error::Runtime {
                    token: operator.clone(),
                    message: "Operands must be two strings or two numbers".into(),
                }),
            },
            TokenType::EqualEqual => Ok(Object::Boolean(self.is_equal(l, r))),
            TokenType::BangEqual => Ok(Object::Boolean(self.is_equal(l, r))),
            _ => unreachable!(),
        }
    }

    fn visit_grouping_expr(&self, expression: &Expr) -> Result<Object, Error> {
        self.evaluate(expression)
    }

    fn visit_literal_expr(&self, value: &Object) -> Result<Object, Error> {
        match value {
            Object::Boolean(b) => Ok(Object::Boolean(*b)),
            Object::Null => Ok(Object::Null),
            Object::Number(n) => Ok(Object::Number(*n)),
            Object::String(s) => Ok(Object::String(s.to_string())),
        }
    }

    fn visit_unary_expr(&self, operator: &Token, right: &Expr) -> Result<Object, Error> {
        let right = self.evaluate(right)?;

        match operator.token_type {
            TokenType::Bang => Ok(Object::Boolean(!self.is_truthy(&right))),
            TokenType::Minus => match right {
                Object::Number(n) => Ok(Object::Number(-n.clone())),
                _ => Ok(Object::Null),
            },
            _ => Ok(Object::Null),
        }
    }
}
