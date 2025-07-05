use ast::{BinOp, LiteralValue, Parameter, TypeAnnotation, UnaryOp};

pub type TypedAstNodeId = usize;

#[derive(Debug, Clone)]
pub enum TypedAstNode {
    BinaryExpression {
        left: TypedAstNodeId,
        op: BinOp,
        right: TypedAstNodeId,
        result_type: Type,
    },
    Literal {
        value: LiteralValue,
        literal_type: Type,
    },
    Program {
        statements: Vec<TypedAstNodeId>,
    },

    LetStatement {
        identifier: String,
        declared_type: Type,
        value: TypedAstNodeId,
    },
    ReturnStatement {
        value: TypedAstNodeId,
        return_type: Type,
    },
    FunctionDefinition {
        name: String,
        parameters: Vec<TypedParameter>,
        return_type: Type,
        body: TypedAstNodeId,
        function_type: Type,
    },
    Block {
        statements: Vec<TypedAstNodeId>,
    },
    Identifier {
        name: String,
        resolved_type: Type,
    },
    Comment(String),
    ForLoop {
        initializer: TypedAstNodeId,
        condition: TypedAstNodeId,
        increment: TypedAstNodeId,
        block: TypedAstNodeId,
    },
    UnaryExpression {
        op: UnaryOp,
        expression: TypedAstNodeId,
        result_type: Type,
    },
    FunctionCall {
        callee: String,
        arguments: Vec<TypedAstNodeId>,
        return_type: Type,
    },
}

#[derive(Debug, Clone)]
pub struct TypedParameter {
    pub name: String,
    pub param_type: Type,
}

impl From<String> for Type {
    fn from(value: String) -> Self {
        match &value.as_str() {
            &"i64" => Type::I64,
            &"i32" => Type::I32,
            &"f64" => Type::I64,
            &"string" => Type::String,
            &"f32" => Type::F32,
            &"void" => Type::Void,
            y => Type::CustomType(y.to_string()),
        }
    }
}
impl From<&Parameter> for TypedParameter {
    fn from(param: &Parameter) -> Self {
        TypedParameter {
            name: param.name.clone(),
            param_type: Type::from(&param.ty),
        }
    }
}

impl From<&LiteralValue> for Type {
    fn from(value: &LiteralValue) -> Self {
        match value {
            LiteralValue::RawString(_) => Type::String,
            LiteralValue::Int(_) => Type::I64,
            LiteralValue::Float(_) => Type::F64,
            LiteralValue::Bool(_) => Type::Bool,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    I64,
    F64,
    I32,
    F32,
    String,
    Bool,
    Void,
    CustomType(String),
    Function {
        params: Vec<Type>,
        return_type: Box<Type>,
    },
    Any,
}

impl From<&TypeAnnotation> for Type {
    fn from(value: &TypeAnnotation) -> Self {
        match value.ty.as_str() {
            "i32" => Type::I32,
            "i64" => Type::I64,
            "f32" => Type::F32,
            "f64" => Type::F64,
            "string" => Type::String,
            "bool" => Type::Bool,
            "void" => Type::Void,
            custom => Type::CustomType(custom.to_string()),
        }
    }
}

impl TypedAstNode {
    pub fn get_type(&self) -> Option<&Type> {
        match self {
            TypedAstNode::BinaryExpression { result_type, .. } => Some(result_type),
            TypedAstNode::Literal { literal_type, .. } => Some(literal_type),
            TypedAstNode::LetStatement { declared_type, .. } => Some(declared_type),
            TypedAstNode::ReturnStatement { return_type, .. } => Some(return_type),
            TypedAstNode::FunctionDefinition { function_type, .. } => Some(function_type),
            TypedAstNode::Identifier { resolved_type, .. } => Some(resolved_type),
            TypedAstNode::UnaryExpression { result_type, .. } => Some(result_type),
            TypedAstNode::FunctionCall { return_type, .. } => Some(return_type),
            // These nodes don't have types
            TypedAstNode::Program { .. } => None,
            TypedAstNode::Block { .. } => None,
            TypedAstNode::Comment(_) => None,
            TypedAstNode::ForLoop { .. } => None,
        }
    }
}
