use std::{collections::HashMap, vec};

use crate::ast::{AstNode, AstNodeId, BinOp, ComparisonOp};

#[derive(Debug)]
pub struct Interpreter {
    ast: Vec<AstNode>,
    roots: Vec<AstNodeId>,
    env_stack: Vec<HashMap<String, EvalOutcome>>,
    structs: HashMap<String, StructType>,
    functions: HashMap<String, Function>,
    current_impl_struct: Option<String>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub params: Vec<String>,
    pub body: Vec<AstNodeId>,
}

#[derive(Debug, Clone)]
pub struct StructType {
    pub name: String,
    pub fields: Vec<String>,
    pub methods: HashMap<String, Function>,
    pub static_methods: HashMap<String, Function>,
}

#[derive(Debug, Clone)]
pub struct StructInstance {
    pub struct_type: String,
    pub fields: HashMap<String, EvalOutcome>,
}

#[derive(Debug)]
pub enum EvalError {
    UndefinedVariable(String),
    NotImplemented(String),
    ParseError(String),
    VariableAlreadyDefined(String),
    UndefinedFunction(String),
    InvalidFunctionArguments(String),
    StackOverflow(String),
}

#[derive(Debug, Clone)]
pub enum Value {
    Int(i64),
    Float(f64),
    String(String),
    Boolean(bool),
    Unit,
    Struct(StructInstance),
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
            structs: HashMap::new(),
            functions: HashMap::new(),
            current_impl_struct: None,
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
            structs: HashMap::new(),
            env_stack: vec![env],
            functions,
            current_impl_struct: None,
        }
    }

    fn with_new_scope<F>(&mut self, f: F) -> Result<EvalOutcome, EvalError>
    where
        F: FnOnce(&mut Interpreter) -> Result<EvalOutcome, EvalError>,
    {
        self.env_stack.push(HashMap::new());
        let result = f(self);
        self.env_stack.pop();
        result
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

    pub fn lookup_variable(&self, name: &str) -> Option<EvalOutcome> {
        for env in self.env_stack.iter().rev() {
            if let Some(value) = env.get(name) {
                return Some(value.clone());
            }
        }
        None
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
                .lookup_variable(&name)
                .ok_or_else(|| EvalError::UndefinedVariable(name.clone())),
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

                let current_env = self.current_env_mut();

                if current_env.contains_key(&name) {
                    return Err(EvalError::VariableAlreadyDefined(format!(
                        "Variable {} is already defined in current scope",
                        name
                    )));
                };

                current_env.insert(name, val);

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
                            Value::Struct(struct_instance) => {
                                write!(output_string, "Struct({})", struct_instance.struct_type)
                                    .unwrap();
                                for (field, value) in struct_instance.fields {
                                    write!(output_string, " {}: {:?}", field, value).unwrap();
                                }
                            }
                        },
                    }
                }
                println!("{}", output_string);

                Ok(EvalOutcome::Value(Value::Unit))
            }
            AstNode::Reassignment(var, expr_id) => {
                let val = self.eval_node(expr_id)?;

                self.reassign_variable(&var, val)?;

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

                let new_env: HashMap<String, EvalOutcome> = params
                    .iter()
                    .zip(arg_values.clone())
                    .map(|(param, value)| (param.clone(), value))
                    .collect();

                self.with_new_scope(|interpreter| {
                    interpreter.current_env_mut().extend(new_env);

                    if interpreter.env_stack.len() > 100 {
                        return Err(EvalError::StackOverflow(
                            "Maximum stack depth exceeded".into(),
                        ));
                    }

                    let mut return_value = Value::Unit;

                    for &stmt_id in &body {
                        match interpreter.eval_node(stmt_id)? {
                            EvalOutcome::Value(_) => continue,
                            EvalOutcome::Return(value) => {
                                return_value = value;
                                break;
                            }
                        }
                    }

                    Ok(EvalOutcome::Value(return_value))
                })
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
            } => self.with_new_scope(|interpreter| {
                let init_value = interpreter.eval_node(init)?;
                if let EvalOutcome::Value(Value::Unit) = init_value {
                } else {
                    return Err(EvalError::ParseError(
                        "For loop initialization did not evaluate to a unit".into(),
                    ));
                }

                let mut result = EvalOutcome::Value(Value::Unit);

                while let EvalOutcome::Value(Value::Boolean(true)) =
                    interpreter.eval_node(condition)?
                {
                    for &stmt_id in &body {
                        match interpreter.eval_node(stmt_id)? {
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

                    interpreter.eval_node(increment)?;
                }

                Ok(result)
            }),
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
            AstNode::Block(items) => self.with_new_scope(|interpreter| {
                let mut result = EvalOutcome::Value(Value::Unit);

                for &item in &items {
                    match interpreter.eval_node(item)? {
                        EvalOutcome::Value(_) => continue,
                        EvalOutcome::Return(val) => {
                            result = EvalOutcome::Return(val);
                            break;
                        }
                    }
                }

                Ok(result)
            }),
            AstNode::StructDeclaration { name, fields } => {
                if self.structs.contains_key(&name) {
                    return Err(EvalError::VariableAlreadyDefined(format!(
                        "Struct {} is already defined",
                        name
                    )));
                }

                let struct_type = StructType {
                    name: name.clone(),
                    fields: fields.clone(),
                    methods: HashMap::new(),
                    static_methods: HashMap::new(),
                };

                self.structs.insert(name, struct_type);

                Ok(EvalOutcome::Value(Value::Unit))
            }
            AstNode::StructImpl { name, methods } => {
                if !self.structs.contains_key(&name) {
                    return Err(EvalError::UndefinedVariable(format!(
                        "Struct {} is not defined",
                        name
                    )));
                };

                self.current_impl_struct = Some(name.clone());

                for &method_id in &methods {
                    self.eval_node(method_id)?;
                }

                self.current_impl_struct = None;

                Ok(EvalOutcome::Value(Value::Unit))
            }
            AstNode::StructMethodCall {
                struct_instance,
                method_name,
                args,
            } => {
                let instance_value = self.eval_node(struct_instance)?;

                let struct_instance = match instance_value {
                    EvalOutcome::Value(Value::Struct(struct_instance)) => struct_instance,
                    _ => {
                        return Err(EvalError::ParseError(format!(
                            "Cannot call method on a non struct value"
                        )));
                    }
                };

                let struct_type =
                    self.structs
                        .get(&struct_instance.struct_type)
                        .ok_or_else(|| {
                            EvalError::UndefinedVariable(format!(
                                "Struct is not defined {}",
                                struct_instance.struct_type
                            ))
                        })?;

                let method = struct_type
                    .methods
                    .get(&method_name)
                    .ok_or_else(|| {
                        EvalError::UndefinedFunction(format!(
                            "Method {} not found on struct {}",
                            method_name, struct_instance.struct_type
                        ))
                    })?
                    .clone();

                let mut arg_values = Vec::with_capacity(args.len());

                for arg_node in args.clone() {
                    let v = self.eval_node(arg_node)?;
                    arg_values.push(v);
                }

                // Check argument count (excluding 'self' parameter)
                if method.params.len() != arg_values.len() + 1 {
                    return Err(EvalError::InvalidFunctionArguments(format!(
                        "Method {}::{} expects {} arguments, got {}",
                        struct_instance.struct_type,
                        method_name,
                        method.params.len() - 1,
                        args.len()
                    )));
                }

                let mut new_env = HashMap::new();
                new_env.insert(
                    "self".to_string(),
                    EvalOutcome::Value(Value::Struct(struct_instance)),
                );

                for (param, value) in method.params.iter().skip(1).zip(arg_values) {
                    new_env.insert(param.clone(), value);
                }

                self.with_new_scope(|interpreter| {
                    interpreter.current_env_mut().extend(new_env);

                    if interpreter.env_stack.len() > 100 {
                        return Err(EvalError::StackOverflow(
                            "Maximum stack depth exceeded".into(),
                        ));
                    }

                    let mut return_value = Value::Unit;

                    for &stmt_id in &method.body {
                        match interpreter.eval_node(stmt_id)? {
                            EvalOutcome::Value(_) => continue,
                            EvalOutcome::Return(value) => {
                                return_value = value;
                                break;
                            }
                        }
                    }

                    Ok(EvalOutcome::Value(return_value))
                })
            }
            AstNode::StructMethod {
                name,
                params,
                body,
                is_static,
            } => {
                let struct_name = self.current_impl_struct.clone().ok_or_else(|| {
                    EvalError::NotImplemented(
                        "Struct method defined outside of struct implementation".to_string(),
                    )
                })?;

                let function = Function {
                    params: params.clone(),
                    body: body.clone(),
                };

                let struct_type = self.structs.get_mut(&struct_name).ok_or_else(|| {
                    EvalError::UndefinedVariable(format!("Struct {} is not defined", struct_name))
                })?;

                if is_static {
                    struct_type.static_methods.insert(name.clone(), function);
                } else {
                    struct_type.methods.insert(name.clone(), function);
                }

                Ok(EvalOutcome::Value(Value::Unit))
            }
            AstNode::StructStaticCall {
                struct_name,
                method_name,
                args,
            } => {
                let struct_type = self.structs.get(&struct_name).ok_or_else(|| {
                    EvalError::UndefinedVariable(format!("Struct {} is not defined", struct_name))
                })?;

                let method = struct_type
                    .static_methods
                    .get(&method_name)
                    .ok_or_else(|| {
                        EvalError::UndefinedFunction(format!(
                            "Static method {}::{} is not defined",
                            struct_name, method_name
                        ))
                    })?
                    .clone();

                if method.params.len() != args.len() {
                    return Err(EvalError::InvalidFunctionArguments(format!(
                        "Static method {}::{} expects {} arguments, got {}",
                        struct_name,
                        method_name,
                        method.params.len(),
                        args.len()
                    )));
                }

                let mut arg_values = Vec::with_capacity(args.len());

                for arg_node in args {
                    let v = self.eval_node(arg_node)?;
                    arg_values.push(v);
                }

                let new_env: HashMap<String, EvalOutcome> = method
                    .params
                    .iter()
                    .zip(arg_values.clone())
                    .map(|(param, value)| (param.clone(), value))
                    .collect();

                self.with_new_scope(|interpreter| {
                    interpreter.current_env_mut().extend(new_env);

                    if interpreter.env_stack.len() > 100 {
                        return Err(EvalError::StackOverflow(
                            "Maximum stack depth exceeded".into(),
                        ));
                    }

                    let mut return_value = Value::Unit;

                    for &stmt_id in &method.body {
                        match interpreter.eval_node(stmt_id)? {
                            EvalOutcome::Value(_) => continue,
                            EvalOutcome::Return(value) => {
                                return_value = value;
                                break;
                            }
                        }
                    }

                    Ok(EvalOutcome::Value(return_value))
                })
            }
            AstNode::StructInit {
                struct_name,
                fields,
            } => {
                let mut field_values = HashMap::new();

                for (field, value_node) in fields {
                    let value = self.eval_node(value_node)?;
                    if let EvalOutcome::Value(Value::Unit) = value {
                        return Err(EvalError::ParseError(format!(
                            "Field {} in struct {} cannot be initialized with unit",
                            field, struct_name
                        )));
                    }
                    field_values.insert(field, value);
                }

                let struct_type = self.structs.get(&struct_name).ok_or_else(|| {
                    EvalError::UndefinedVariable(format!("Struct {} is not defined", struct_name))
                })?;

                for field in &struct_type.fields {
                    if !field_values.contains_key(field) {
                        return Err(EvalError::ParseError(format!(
                            "Field {} in struct {} is not initialized",
                            field, struct_name
                        )));
                    }
                }

                Ok(EvalOutcome::Value(Value::Struct(StructInstance {
                    struct_type: struct_name.clone(),
                    fields: field_values,
                })))
            }
            AstNode::StructFieldAccess { instance, field } => {
                let instance_value = self.eval_node(instance)?;

                match instance_value {
                    EvalOutcome::Value(Value::Struct(struct_instance)) => {
                        match struct_instance.fields.get(&field) {
                            Some(value) => Ok(value.clone()),
                            None => Err(EvalError::UndefinedVariable(format!(
                                "Struct does not contain definition for {}",
                                field,
                            ))),
                        }
                    }
                    _ => Err(EvalError::ParseError(format!(
                        "Cannot access field on a non struct value"
                    ))),
                }
            }
            AstNode::StructFieldAssignment {
                instance,
                field,
                value,
            } => {
                let new_value = match self.eval_node(value)? {
                    EvalOutcome::Return(_) => {
                        return Err(EvalError::ParseError(format!(
                            "Return not allowed in field assignment"
                        )));
                    }
                    EvalOutcome::Value(value) => value,
                };

                let instance_value = self.ast[instance].clone();

                match instance_value {
                    AstNode::Var(var_name) => {
                        let current_value = self.lookup_variable(&var_name).ok_or_else(|| {
                            EvalError::UndefinedVariable(format!(
                                "Variable {} is not defined",
                                var_name
                            ))
                        })?;

                        match current_value {
                            EvalOutcome::Value(Value::Struct(mut struct_instance)) => {
                                if !struct_instance.fields.contains_key(&field) {
                                    return Err(EvalError::UndefinedVariable(format!(
                                        "Field {} doesn't exist on struct {}",
                                        field, struct_instance.struct_type
                                    )));
                                }

                                struct_instance
                                    .fields
                                    .insert(field, EvalOutcome::Value(new_value));

                                Ok(EvalOutcome::Value(Value::Struct(struct_instance)))
                            }
                            _ => Err(EvalError::ParseError(format!(
                                "Cannot assign fields on non-struct value"
                            ))),
                        }
                    }
                    _ => Err(EvalError::ParseError(format!(
                        "Can only assign fields on struct variables"
                    ))),
                }
            }
        }
    }

    fn reassign_variable(&mut self, var: &str, val: EvalOutcome) -> Result<(), EvalError> {
        for env in self.env_stack.iter_mut().rev() {
            if env.contains_key(var) {
                env.insert(var.to_string(), val);
                return Ok(());
            }
        }

        Err(EvalError::UndefinedVariable(format!(
            "Variable {} is not defined in scope",
            var
        )))
    }
}
