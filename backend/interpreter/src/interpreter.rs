use std::{collections::HashMap, vec};

use crate::ast::{AstNode, AstNodeId, BinOp, ComparisonOp};

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
            .map(|node| node.clone())
            .ok_or_else(|| EvalError::ParseError("Invalid node ID".to_string()))?;

        match node_kind {
            AstNode::Number(n) => Ok(EvalOutcome::Value(Value::Int(n))),
            AstNode::Var(name) => self
                .get_environment()
                .get(&name)
                .cloned()
                .ok_or_else(|| EvalError::UndefinedVariable(name)),
            AstNode::RawString(s) => Ok(EvalOutcome::Value(Value::String(s))),
            AstNode::BinaryExpression(l, operator, r) => {
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
                                BinOp::Modulo => x % y,
                                _ => {
                                    return Err(EvalError::ParseError(format!(
                                        "Cannot evaluate integer operation {:?} {:?} {:?}",
                                        x, operator, y
                                    )));
                                }
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
            AstNode::Assign(name, expr_id) => {
                let val = self.eval_node(expr_id)?;

                self.current_env_mut().insert(name, val);

                Ok(EvalOutcome::Value(Value::Unit))
            }
            AstNode::Pow(_, _) => Err(EvalError::NotImplemented(
                "Pow operator not implemented yet".to_string(),
            )),
            AstNode::Print(exprs) => {
                use std::fmt::Write;
                let mut output_string = String::new();
                for expr in exprs {
                    let value = self.eval_node(expr)?;

                    match value {
                        EvalOutcome::Value(value) | EvalOutcome::Return(value) => match value {
                            Value::Unit => {}
                            Value::Float(x) => write!(output_string, "{}", x).unwrap(),
                            Value::Int(x) => write!(output_string, "{}", x).unwrap(),
                            Value::String(x) => write!(output_string, "{}", x).unwrap(),
                            Value::Boolean(bool) => {
                                write!(output_string, "{}", bool.to_string()).unwrap()
                            }
                        },
                    }
                }
                println!("{}", output_string);

                Ok(EvalOutcome::Value(Value::Unit))
            }
            AstNode::Reassignment(var, expr_id) => {
                if !self.get_environment().contains_key(&var) {
                    return Err(EvalError::UndefinedVariable(format!(
                        "Variable {var} is undefined."
                    )));
                }

                let val = self.eval_node(expr_id)?;

                self.current_env_mut().insert(var, val);

                Ok(EvalOutcome::Value(Value::Unit))
            }
            AstNode::FunctionDeclaration { name, args, block } => {
                self.functions.insert(
                    name,
                    Function {
                        params: args,
                        body: block,
                    },
                );

                Ok(EvalOutcome::Value(Value::Unit))
            }
            AstNode::FunctionCall { func, args } => {
                let fn_name = match &self.ast[func] {
                    AstNode::Var(name) => name.as_str(),
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
            AstNode::Return(x) => {
                let v = match self.eval_node(x)? {
                    EvalOutcome::Value(v) => v,
                    EvalOutcome::Return(inner) => {
                        return Ok(EvalOutcome::Return(inner));
                    }
                };
                Ok(EvalOutcome::Return(v))
            }
            AstNode::If {
                expression,
                block,
                else_if_blocks,
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

                let stmts = if truth {
                    &block
                } else {
                    let else_if_block_to_execute = else_if_blocks.as_ref().and_then(|blocks| {
                        blocks.iter().find_map(|(cond, stmts)| {
                            if let EvalOutcome::Value(Value::Boolean(true)) =
                                self.eval_node(*cond).unwrap()
                            {
                                Some(stmts)
                            } else {
                                None
                            }
                        })
                    });

                    if let Some(stmts) = else_if_block_to_execute {
                        stmts
                    } else {
                        static EMPTY_VEC: Vec<AstNodeId> = Vec::new();
                        else_block.as_ref().unwrap_or(&EMPTY_VEC)
                    }
                };

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
            AstNode::Equality(x, y) => {
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
            AstNode::ForLoop {
                init,
                condition,
                increment,
                body,
            } => {
                let init_value = self.eval_node(init)?;
                if let EvalOutcome::Value(Value::Unit) = init_value {
                } else {
                    return Err(EvalError::ParseError(
                        "For loop initialization did not evaluate to a unit".into(),
                    ));
                }

                let mut result = EvalOutcome::Value(Value::Unit);

                while let EvalOutcome::Value(Value::Boolean(true)) = self.eval_node(condition)? {
                    for &stmt_id in &body {
                        match self.eval_node(stmt_id)? {
                            EvalOutcome::Value(_) => continue,
                            EvalOutcome::Return(val) => {
                                result = EvalOutcome::Return(val);
                                break;
                            }
                        }
                    }

                    if let EvalOutcome::Return(_) = result {
                        break;
                    }

                    self.eval_node(increment)?;
                }

                Ok(result)
            }
            AstNode::PostIncrement(increment) => {
                if let Some(EvalOutcome::Value(Value::Int(mut value))) =
                    self.get_environment().get(&increment).cloned()
                {
                    value += 1;
                    self.current_env_mut()
                        .insert(increment, EvalOutcome::Value(Value::Int(value)));
                    Ok(EvalOutcome::Value(Value::Int(value - 1)))
                } else {
                    Err(EvalError::UndefinedVariable(increment))
                }
            }
            AstNode::Comparison(x, op, y) => match (self.eval_node(x)?, self.eval_node(y)?) {
                (EvalOutcome::Value(Value::Int(lhs)), EvalOutcome::Value(Value::Int(rhs))) => {
                    let result = match op {
                        ComparisonOp::LessOrEqual => lhs <= rhs,
                        ComparisonOp::Less => lhs < rhs,
                        ComparisonOp::GreaterOrEqual => lhs >= rhs,
                        ComparisonOp::Greater => lhs > rhs,
                        _ => {
                            return Err(EvalError::ParseError(
                                "Comparison does not evaluate to boolean".into(),
                            ));
                        }
                    };
                    Ok(EvalOutcome::Value(Value::Boolean(result)))
                }
                (
                    EvalOutcome::Value(Value::Boolean(lhs)),
                    EvalOutcome::Value(Value::Boolean(rhs)),
                ) => match op {
                    ComparisonOp::Or => Ok(EvalOutcome::Value(Value::Boolean(lhs || rhs))),
                    ComparisonOp::And => Ok(EvalOutcome::Value(Value::Boolean(lhs && rhs))),
                    _ => Err(EvalError::ParseError(
                        "Comparison does not evaluate to boolean this".into(),
                    )),
                },
                _ => Err(EvalError::ParseError(
                    "Comparison does not evaluate to boolean parse error".into(),
                )),
            },
        }
    }
}
