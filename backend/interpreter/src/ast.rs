#[derive(Debug, Clone)]
pub enum AstNode {
    Number(i64),
    Var(String),
    RawString(String),
    BinaryExpression(AstNodeId, BinOp, AstNodeId),
    PostIncrement(String),
    Assign(String, AstNodeId),
    Reassignment(String, AstNodeId),
    Pow(AstNodeId, AstNodeId),
    Print(Vec<AstNodeId>),
    FunctionDeclaration {
        name: String,
        args: Vec<String>,
        block: Vec<AstNodeId>,
    },
    FunctionCall {
        func: AstNodeId,
        args: Vec<AstNodeId>,
    },
    Block(Vec<AstNodeId>),
    If {
        expression: AstNodeId,
        block: Vec<AstNodeId>,
        else_if_blocks: Option<Vec<(AstNodeId, Vec<AstNodeId>)>>,
        else_block: Option<Vec<AstNodeId>>,
    },
    Return(AstNodeId),
    Equality(AstNodeId, AstNodeId),
    Comparison(AstNodeId, ComparisonOp, AstNodeId),
    ForLoop {
        init: AstNodeId,
        condition: AstNodeId,
        increment: AstNodeId,
        body: Vec<AstNodeId>,
    },
    StructDeclaration {
        name: String,
        fields: Vec<String>,
    },
    StructImpl {
        name: String,
        methods: Vec<AstNodeId>,
    },
    StructMethod {
        name: String,
        params: Vec<String>,
        body: Vec<AstNodeId>,
        is_static: bool,
    },
    StructMethodCall {
        struct_instance: AstNodeId,
        method_name: String,
        args: Vec<AstNodeId>,
    },
    StructStaticCall {
        struct_name: String,
        method_name: String,
        args: Vec<AstNodeId>,
    },
    StructInit {
        struct_name: String,
        fields: Vec<(String, AstNodeId)>,
    },
    StructFieldAccess {
        instance: AstNodeId,
        field: String,
    },
    StructFieldAssignment {
        instance: AstNodeId,
        field: String,
        value: AstNodeId,
    },
}

#[derive(Debug, Clone)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Exp,
    Modulo,
}

#[derive(Debug, Clone)]
pub enum ComparisonOp {
    LessOrEqual,
    Less,
    GreaterOrEqual,
    Greater,
    Or,
    And,
}

pub type AstNodeId = usize;
