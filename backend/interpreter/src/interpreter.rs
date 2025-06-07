use std::collections::HashMap;

use crate::ast::{AstKind, AstNode, AstNodeId, BinOp};

#[derive(Debug)]
pub struct Interpreter {
    ast: Vec<AstNode>,
    roots: Vec<AstNodeId>,
    env_stack: Vec<HashMap<String, EvalOutcome>>,
    functions: HashMap<String, Function>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub params: Vec<String>,
    pub body: Vec<AstNodeId>,
}

#[derive(Debug)]
pub enum EvalError {
    UndefinedVariable(String),
    NotImplemented(String),
    ParseError(String),
    VariableAlreadyDefined(String),
    UndefinedFunction(String),
    InvalidFunctionArguments(String),
}

#[derive(Debug, Clone)]
pub enum Value {
    Int(i64),
    Float(f64),
    String(String),
    Boolean(bool),
    Unit,
}

#[derive(Debug, Clone)]
pub enum EvalOutcome {
    Value(Value),
    Return(Value),
}

impl Interpreter {
    pub fn new(ast: Vec<AstNode>, roots: Vec<AstNodeId>) -> Self {
        Self {
            ast,
            roots,
            env_stack: vec![HashMap::new()],
            functions: HashMap::new(),
        }
    }

    pub fn new_with_env(
        ast: Vec<AstNode>,
        roots: Vec<AstNodeId>,
        env: HashMap<String, EvalOutcome>,
        functions: HashMap<String, Function>,
    ) -> Self {
        Self {
            ast,
            roots,
            env_stack: vec![env],
            functions,
        }
    }

    pub fn run(&mut self) -> Result<(), EvalError> {
        let roots = self.roots.clone();
        for stmt in roots {
            match self.eval_node(stmt) {
                Ok(_) => {}
                Err(err) => println!("{:?}", err),
            }
        }
        Ok(())
    }

    pub fn into_functions(&self) -> HashMap<String, Function> {
        self.functions.clone()
    }

    fn current_env_mut(&mut self) -> &mut HashMap<String, EvalOutcome> {
        self.env_stack.last_mut().unwrap()
    }

    pub fn get_environment(&self) -> HashMap<String, EvalOutcome> {
        self.env_stack.last().unwrap().clone()
    }

    pub fn get_ast(&self) -> Vec<AstNode> {
        self.ast.clone()
    }

    fn eval_node(&mut self, node_id: AstNodeId) -> Result<EvalOutcome, EvalError> {
        let node_kind = self
            .ast
            .get(node_id)
            .map(|node| node.kind.clone())
            .ok_or_else(|| EvalError::ParseError("Invalid node ID".to_string()))?;

        match node_kind {
            AstKind::Number(n) => Ok(EvalOutcome::Value(Value::Int(n))),
            AstKind::Var(name) => self
                .get_environment()
                .get(&name)
                .cloned()
                .ok_or_else(|| EvalError::UndefinedVariable(name)),
            AstKind::RawString(s) => Ok(EvalOutcome::Value(Value::String(s))),
            AstKind::BinaryExpression(l, operator, r) => {
                let lhs = self.eval_node(l)?;
                let rhs = self.eval_node(r)?;

                match (lhs, rhs) {
                    (EvalOutcome::Value(x), EvalOutcome::Value(y)) => match (x, y) {
                        (Value::Int(x), Value::Int(y)) => {
                            return Ok(EvalOutcome::Value(Value::Int(match operator {
                                BinOp::Add => x + y,
                                BinOp::Sub => x - y,
                                BinOp::Mul => x * y,
                                BinOp::Div => x / y,
                                BinOp::Exp => x ^ y,
                            })));
                        }

                        (x, y) => {
                            return Err(EvalError::ParseError(format!(
                                "Cannot evaluate expression {:?} {:?} {:?}",
                                x, operator, y
                            )));
                        }
                    },
                    (_, _) => return Ok(EvalOutcome::Value(Value::Unit)),
                }
            }
            AstKind::Assign(name, expr_id) => {
                if self.get_environment().contains_key(&name) {
                    return Err(EvalError::VariableAlreadyDefined(format!(
                        "Variable {name} is already defined in the same scope"
                    )));
                }

                let val = self.eval_node(expr_id)?;

                self.current_env_mut().insert(name, val);

                Ok(EvalOutcome::Value(Value::Unit))
            }
            AstKind::Pow(_, _) => Err(EvalError::NotImplemented(
                "Pow operator not implemented yet".to_string(),
            )),
            AstKind::Print(expr_id) => {
                let value = self.eval_node(expr_id)?;

                match value {
                    EvalOutcome::Value(value) | EvalOutcome::Return(value) => match value {
                        Value::Unit => {}
                        Value::Float(x) => println!("{}", x),
                        Value::Int(x) => println!("{}", x),
                        Value::String(x) => println!("{}", x),
                        Value::Boolean(bool) => println!("{}", bool.to_string()),
                    },
                }

                Ok(EvalOutcome::Value(Value::Unit))
            }
            AstKind::Reassignment(var, expr_id) => {
                if !self.get_environment().contains_key(&var) {
                    return Err(EvalError::UndefinedVariable(format!(
                        "Variable {var} is undefined."
                    )));
                }

                let val = self.eval_node(expr_id)?;

                self.current_env_mut().insert(var, val);

                Ok(EvalOutcome::Value(Value::Unit))
            }
            AstKind::FunctionDeclaration { name, args, block } => {
                self.functions.insert(
                    name,
                    Function {
                        params: args,
                        body: block,
                    },
                );

                Ok(EvalOutcome::Value(Value::Unit))
            }
            AstKind::FunctionCall { func, args } => {
                let fn_name = match &self.ast[func].kind {
                    AstKind::Var(name) => name.as_str(),
                    other => panic!("Cannot call non-function value: {:?}", other),
                };

                let Function { params, body } = match self.functions.get(fn_name) {
                    Some(function) => function,
                    None => {
                        return Err(EvalError::UndefinedFunction(format!(
                            "Function {} is undefined",
                            fn_name
                        )));
                    }
                }
                .clone();

                let mut arg_values = Vec::with_capacity(args.len());

                for arg_node in args.clone() {
                    let v = self.eval_node(arg_node)?;
                    arg_values.push(v);
                }

                if arg_values.len() != args.len() {
                    return Err(EvalError::InvalidFunctionArguments(format!(
                        "Function arguments are invalid"
                    )));
                }

                self.env_stack.push(HashMap::new());
                {
                    let local = self.env_stack.last_mut().unwrap();
                    for (param, value) in params.iter().zip(arg_values) {
                        local.insert(param.clone(), value);
                    }
                }

                let mut return_value = Value::Unit;

                for &stmt_id in &body {
                    match self.eval_node(stmt_id)? {
                        EvalOutcome::Value(_) => continue,
                        EvalOutcome::Return(value) => {
                            return_value = value;
                            break;
                        }
                    }
                }

                self.env_stack.pop();

                Ok(EvalOutcome::Value(return_value))
            }
            AstKind::Return(x) => {
                let v = match self.eval_node(x)? {
                    EvalOutcome::Value(v) => v,
                    EvalOutcome::Return(inner) => {
                        return Ok(EvalOutcome::Return(inner));
                    }
                };
                Ok(EvalOutcome::Return(v))
            }
            AstKind::If {
                expression,
                block,
                else_block,
            } => {
                let cond_outcome = self.eval_node(expression)?;
                let truth = match cond_outcome {
                    EvalOutcome::Value(Value::Boolean(b)) => b,
                    EvalOutcome::Return(v) => return Ok(EvalOutcome::Return(v)),
                    _ => {
                        return Err(EvalError::ParseError(
                            "Condition did not evaluate to a boolean".into(),
                        ));
                    }
                };

                let stmts = if truth { &block } else { &else_block };
                let mut result = EvalOutcome::Value(Value::Unit);

                for &stmt_id in stmts {
                    match self.eval_node(stmt_id)? {
                        EvalOutcome::Value(_) => continue,
                        EvalOutcome::Return(val) => {
                            result = EvalOutcome::Return(val);
                            break;
                        }
                    }
                }

                Ok(result)
            }
            AstKind::Equality(x, y) => {
                let lhs = self.eval_node(x)?;
                let rhs = self.eval_node(y)?;

                return Ok(match (lhs, rhs) {
                    (EvalOutcome::Value(x), EvalOutcome::Value(y)) => match (x, y) {
                        (Value::Int(x), Value::Int(y)) => {
                            EvalOutcome::Value(Value::Boolean(x == y))
                        }
                        (Value::String(x), Value::String(y)) => {
                            EvalOutcome::Value(Value::Boolean(x == y))
                        }
                        _ => {
                            return Err(EvalError::ParseError(format!(
                                "Unable to parse expression"
                            )));
                        }
                    },
                    (_, _) => EvalOutcome::Value(Value::Unit),
                });
            }
        }
    }
}
