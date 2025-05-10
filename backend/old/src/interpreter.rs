use crate::{
    error::Error,
    expression::{Expr, Object},
    syntax::Stmt,
    token::{Token, TokenType},
};

pub struct Interpreter;

impl Interpreter {
    pub fn evaluate(&self, statement: &Stmt) -> Result<Object, Error> {
        match statement {
            Stmt::Expression { expression } => self.evaluate_expr(expression),
            Stmt::Print { expression } => self.evaluate_print(expression),
        }
    }

    fn evaluate_print(&self, expression: &Expr) -> Result<Object, Error> {
        let expr = self.evaluate_expr(expression)?;

        println!("{}", expr);

        return Ok(Object::Null);
    }

    pub fn evaluate_expr(&self, expression: &Expr) -> Result<Object, Error> {
        let value = match expression {
            Expr::Binary {
                left,
                operator,
                right,
            } => self.evaluate_binary(left, operator, right),
            Expr::Literal { value } => self.evaluate_literal(value),
            Expr::Unary { operator, right } => self.evaluate_unary(operator, right),
            Expr::Grouping { expr } => self.evaluate_expr(expr),
        };

        value
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
            message: "Operands must be a number".to_string(),
        })
    }

    fn evaluate_binary(
        &self,
        left: &Expr,
        operator: &Token,
        right: &Expr,
    ) -> Result<Object, Error> {
        let left_val = self.evaluate_expr(left)?;
        let right_val = self.evaluate_expr(right)?;

        match (left_val, &operator.token_type, right_val) {
            (Object::Number(l), TokenType::Greater, Object::Number(r)) => {
                Ok(Object::Boolean(l > r))
            }
            (Object::Number(l), TokenType::Less, Object::Number(r)) => Ok(Object::Boolean(l < r)),
            (Object::Number(l), TokenType::LessEqual, Object::Number(r)) => {
                Ok(Object::Boolean(l <= r))
            }
            (Object::Number(l), TokenType::GreaterEqual, Object::Number(r)) => {
                Ok(Object::Boolean(l >= r))
            }

            // Arithmetic operations
            (Object::Number(l), TokenType::Minus, Object::Number(r)) => Ok(Object::Number(l - r)),
            (Object::Number(l), TokenType::Star, Object::Number(r)) => Ok(Object::Number(l * r)),
            (Object::Number(l), TokenType::Slash, Object::Number(r)) => Ok(Object::Number(l / r)),

            // Addition (handles both numbers and strings)
            (Object::String(l), TokenType::Plus, Object::String(r)) => {
                Ok(Object::String(format!("{}{}", l, r)))
            }
            (Object::Number(l), TokenType::Plus, Object::Number(r)) => Ok(Object::Number(l + r)),

            // Equality operations
            (l, TokenType::EqualEqual, r) => Ok(Object::Boolean(self.is_equal(l, r))),
            (l, TokenType::BangEqual, r) => Ok(Object::Boolean(!self.is_equal(l, r))),

            // Error cases for numeric operations
            (
                _,
                _op @ (TokenType::Minus
                | TokenType::Star
                | TokenType::Slash
                | TokenType::Greater
                | TokenType::Less
                | TokenType::LessEqual
                | TokenType::GreaterEqual),
                _,
            ) => self.number_operand_error(operator),

            // Error case for addition
            (_, TokenType::Plus, _) => Err(Error::Runtime {
                token: operator.clone(),
                message: "Operands must be two numbers or two strings".into(),
            }),

            // Catch-all for unexpected operators
            _ => unreachable!("Invalid binary operator"),
        }
    }

    fn evaluate_literal(&self, value: &Object) -> Result<Object, Error> {
        Ok(value.clone())
    }

    fn evaluate_unary(&self, operator: &Token, right: &Expr) -> Result<Object, Error> {
        let right_val = self.evaluate_expr(right)?;

        match (&operator.token_type, right_val) {
            (TokenType::Bang, value) => Ok(Object::Boolean(!self.is_truthy(&value))),
            (TokenType::Minus, Object::Number(n)) => Ok(Object::Number(-n)),
            (TokenType::Minus, _) => Err(Error::Runtime {
                token: operator.clone(),
                message: "Operand must be a number".into(),
            }),
            _ => unreachable!("Invalid unary operator"),
        }
    }
}
