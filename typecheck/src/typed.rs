use std::{collections::HashMap, fmt::Display};

use ast::{BinOp, LiteralValue};

#[derive(Debug, Clone)]
pub enum AstNode {
    Program {
        statements: Vec<AstNodeId>,
    },
    FunctionDecl {
        params: Vec<TypedValueType>,
        block: AstNodeId,
        return_type: TypedType,
    },
    Block {
        statements: Vec<AstNodeId>,
    },
    LetStatement {
        value: TypedValueType,
        expr: AstNodeId,
    },
    BinaryExpression(AstNodeId, TypedBinOp, AstNodeId, TypedType),
    UnaryExpression(TypedUnaryOp, AstNodeId),
    StructDecl {
        name: String,
        methods: Vec<AstNodeId>,
        fields: HashMap<String, TypedValueType>,
    },
    StructMethodDecl {
        params: Vec<TypedValueType>,
        block: Vec<AstNodeId>,
        return_type: TypedType,
    },
    FunctionCall {
        args: Vec<AstNodeId>,
    },
    StructMethodCall {},
    Literal(TypedLiteralValue),
}

impl AstNode {
    pub fn get_type(&self) -> Option<&TypedType> {
        match self {
            AstNode::FunctionDecl { return_type, .. } => Some(return_type),
            AstNode::LetStatement { value, .. } => Some(&value.actual_type),
            AstNode::BinaryExpression(_, _, _, expr) => Some(expr),
            AstNode::UnaryExpression(_, _) => None,
            AstNode::StructDecl { .. } => None,
            AstNode::StructMethodDecl { return_type, .. } => Some(return_type),
            AstNode::FunctionCall { .. } => None,
            AstNode::StructMethodCall { .. } => None,
            AstNode::Literal(_) => None,
            _ => None,
        }
    }
}

impl From<TypedType> for Type {
    fn from(value: TypedType) -> Self {
        match value {
            TypedType::TypeValue(x) => match x.as_str() {
                "i64" => Type::I64,
                "i32" => Type::I32,
                "f64" => Type::I64,
                "string" => Type::String,
                "f32" => Type::F32,
                "void" => Type::Void,
                y => Type::CustomType(y.to_owned()),
            },
        }
    }
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

#[derive(Debug, Clone)]
pub enum TypedLiteralValue {
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),
}

pub type AstNodeId = usize;

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct TypedValueType {
    pub name: String,
    pub actual_type: TypedValueType,
}

#[derive(Debug, Clone)]
pub enum TypedType {
    TypeValue(String),
}

#[derive(Debug, Clone)]
pub enum TypedBinOp {
    Add,
    Sub,
    Multiply,
    Divide,
    Modulo,
    Or,
    And,
    NotEqual,
    Equal,
    GreaterEqual,
    LesserEqual,
}

#[derive(Debug, Clone)]
pub enum TypedUnaryOp {
    Plus,
    Minus,
    Not,
    PlusPlus,
    MinusMinus,
}

impl From<&LiteralValue> for TypedLiteralValue {
    fn from(value: &LiteralValue) -> Self {
        match value {
            LiteralValue::Int(i) => TypedLiteralValue::Int(*i),
            LiteralValue::Float(f) => TypedLiteralValue::Float(*f),
            LiteralValue::RawString(s) => TypedLiteralValue::String(s.clone()),
            LiteralValue::Bool(b) => TypedLiteralValue::Bool(*b),
        }
    }
}

impl From<BinOp> for TypedBinOp {
    fn from(op: BinOp) -> Self {
        match op {
            BinOp::Add => TypedBinOp::Add,
            BinOp::Sub => TypedBinOp::Sub,
            BinOp::Multiply => TypedBinOp::Multiply,
            BinOp::Divide => TypedBinOp::Divide,
            BinOp::Modulo => TypedBinOp::Modulo,
            BinOp::Or => TypedBinOp::Or,
            BinOp::And => TypedBinOp::And,
            BinOp::NotEqual => TypedBinOp::NotEqual,
            BinOp::Equal => TypedBinOp::Equal,
            BinOp::LesserEqual => TypedBinOp::LesserEqual,
            BinOp::GreaterEqual => TypedBinOp::GreaterEqual,
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
}
