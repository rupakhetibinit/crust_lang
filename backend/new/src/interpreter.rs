use std::collections::HashMap;

use crate::ast::{AstKind, AstNode, AstNodeId};

#[derive(Debug)]
pub struct Interpreter {
    ast: Vec<AstNode>,
    roots: Vec<AstNodeId>,
    env: HashMap<String, String>,
}

#[derive(Debug)]
pub enum EvalError {
    UndefinedVariable(String),
    NotImplemented(String),
    ParseError(String),
    VariableAlreadyDefined(String),
}

impl Interpreter {
    pub fn new(ast: Vec<AstNode>, roots: Vec<AstNodeId>) -> Self {
        Self {
            ast,
            roots,
            env: HashMap::new(),
        }
    }

    pub fn new_with_env(
        ast: Vec<AstNode>,
        roots: Vec<AstNodeId>,
        env: HashMap<String, String>,
    ) -> Self {
        Self { ast, roots, env }
    }

    pub fn run(&mut self) -> Result<(), EvalError> {
        let roots = self.roots.clone();
        for stmt in roots {
            match self.eval_node(stmt) {
                Ok(out) => {
                    if !out.is_empty() {
                        println!("{}", out);
                    }
                }
                Err(err) => println!("{:?}", err),
            }
        }
        Ok(())
    }

    pub fn get_environment(&self) -> HashMap<String, String> {
        self.env.clone()
    }

    pub fn get_ast(&self) -> Vec<AstNode> {
        self.ast.clone()
    }

    fn eval_node(&mut self, node_id: AstNodeId) -> Result<String, EvalError> {
        let node_kind = self
            .ast
            .get(node_id)
            .map(|node| node.kind.clone())
            .ok_or_else(|| EvalError::ParseError("Invalid node ID".to_string()))?;

        match node_kind {
            AstKind::Number(n) => Ok(n.to_string()),
            AstKind::Var(name) => self
                .env
                .get(&name)
                .cloned()
                .ok_or_else(|| EvalError::UndefinedVariable(name)),
            AstKind::RawString(s) => Ok(s),
            AstKind::Add(l, r) => {
                let lhs = self.eval_node(l)?;
                let rhs = self.eval_node(r)?;

                let lhs_val = lhs.parse::<i64>().map_err(|_| {
                    EvalError::ParseError(format!("Cannot parse '{}' as number", lhs))
                })?;

                let rhs_val = rhs.parse::<i64>().map_err(|_| {
                    EvalError::ParseError(format!("Cannot parse '{}' as number", rhs))
                })?;

                Ok((lhs_val + rhs_val).to_string())
            }
            AstKind::Assign(name, expr_id) => {
                if self.env.contains_key(&name) {
                    return Err(EvalError::VariableAlreadyDefined(format!(
                        "Variable {name} is already defined in the same scope"
                    )));
                }

                let val = self.eval_node(expr_id)?;

                self.env.insert(name, val);

                Ok(String::new())
            }
            AstKind::Sub(l, r) => {
                let lhs = self.eval_node(l)?;
                let rhs = self.eval_node(r)?;

                let lhs_val = lhs.parse::<i64>().map_err(|_| {
                    EvalError::ParseError(format!("Cannot parse '{}' as number", lhs))
                })?;

                let rhs_val = rhs.parse::<i64>().map_err(|_| {
                    EvalError::ParseError(format!("Cannot parse '{}' as number", rhs))
                })?;

                Ok((lhs_val - rhs_val).to_string())
            }
            AstKind::Mul(l, r) => {
                let lhs = self.eval_node(l)?;
                let rhs = self.eval_node(r)?;

                let lhs_val = lhs.parse::<i64>().map_err(|_| {
                    EvalError::ParseError(format!("Cannot parse '{}' as number", lhs))
                })?;

                let rhs_val = rhs.parse::<i64>().map_err(|_| {
                    EvalError::ParseError(format!("Cannot parse '{}' as number", rhs))
                })?;

                Ok((lhs_val * rhs_val).to_string())
            }
            AstKind::Div(l, r) => {
                let lhs = self.eval_node(l)?;
                let rhs = self.eval_node(r)?;

                let lhs_val = lhs.parse::<i64>().map_err(|_| {
                    EvalError::ParseError(format!("Cannot parse '{}' as number", lhs))
                })?;

                let rhs_val = rhs.parse::<i64>().map_err(|_| {
                    EvalError::ParseError(format!("Cannot parse '{}' as number", rhs))
                })?;

                if rhs_val == 0 {
                    return Err(EvalError::ParseError("Division by zero".to_string()));
                }

                Ok((lhs_val / rhs_val).to_string())
            }
            AstKind::Pow(_, _) => Err(EvalError::NotImplemented(
                "Pow operator not implemented yet".to_string(),
            )),
            AstKind::Print(expr_id) => self.eval_node(expr_id),
            AstKind::ReAssign(var, expr_id) => {
                if !self.env.contains_key(&var) {
                    return Err(EvalError::UndefinedVariable(format!(
                        "Variable {var} is undefined."
                    )));
                }

                let val = self.eval_node(expr_id)?;

                self.env.insert(var, val);

                Ok(String::new())
            }
        }
    }
}
