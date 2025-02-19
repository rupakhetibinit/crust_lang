use crate::token::Token;

#[derive(Debug)]
pub enum Expr {
    Binary {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },
    Literal {
        value: Option<String>,
    },
    Unary {
        operator: Token,
        right: Box<Expr>,
    },
    Grouping {
        expr: Box<Expr>,
    },
}

pub trait Visitor<R> {
    fn visit_binary_expr(&self, left: &Expr, operator: &Token, right: &Expr) -> R;
    fn visit_grouping_expr(&self, expression: &Expr) -> R;
    fn visit_literal_expr(&self, value: String) -> R;
    fn visit_unary_expr(&self, operator: &Token, right: &Expr) -> R;
}

impl Expr {
    pub fn accept<R>(&self, visitor: &dyn Visitor<R>) -> R {
        match self {
            Expr::Binary {
                left,
                operator,
                right,
            } => visitor.visit_binary_expr(left, operator, right),
            Expr::Literal { value } => visitor.visit_literal_expr(value.clone().unwrap()),
            Expr::Unary { operator, right } => visitor.visit_unary_expr(operator, right),
            Expr::Grouping { expr } => visitor.visit_grouping_expr(expr),
        }
    }
}

pub struct AstPrinter;
impl AstPrinter {
    pub fn print(&self, expr: Expr) -> String {
        expr.accept(self)
    }
    fn parenthesize(&self, name: String, exprs: Vec<&Expr>) -> String {
        let mut r = String::new();
        r.push('(');
        r.push_str(&name);
        for e in &exprs {
            r.push(' ');
            r.push_str(&e.accept(self));
        }
        r.push(')');
        r
    }
}

impl Visitor<String> for AstPrinter {
    fn visit_binary_expr(&self, left: &Expr, operator: &Token, right: &Expr) -> String {
        self.parenthesize(operator.lexeme.clone(), vec![left, right])
    }
    fn visit_grouping_expr(&self, expr: &Expr) -> String {
        self.parenthesize("group".to_string(), vec![expr])
    }
    fn visit_literal_expr(&self, value: String) -> String {
        value // check for null
    }
    fn visit_unary_expr(&self, operator: &Token, right: &Expr) -> String {
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
                    value: Some("123".to_string()),
                }),
            }),
            operator: Token::new(TokenType::Star, "*".to_string(), Some("*".to_string()), 1),
            right: Box::new(Expr::Grouping {
                expr: Box::new(Expr::Literal {
                    value: Some("45.67".to_string()),
                }),
            }),
        };

        let printer = AstPrinter;
        assert_eq!(
            printer.print(expression),
            format!("(* (- 123) (group 45.67))")
        );
    }
}
