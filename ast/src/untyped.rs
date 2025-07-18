#[derive(Debug, Clone)]
pub enum UntypedAstNode {
    BinaryExpression {
        left: UntypedAstNodeId,
        op: BinOp,
        right: UntypedAstNodeId,
    },
    Literal(LiteralValue),
    Program(Vec<UntypedAstNodeId>),
    LetStatement {
        identifier: String,
        ty: Option<TypeAnnotation>,
        value: UntypedAstNodeId,
    },
    ReturnStatement {
        value: UntypedAstNodeId,
    },
    FunctionDefinition {
        name: String,
        parameters: Vec<Parameter>,
        return_type: TypeAnnotation,
        body: UntypedAstNodeId,
    },

    Block(Vec<UntypedAstNodeId>),
    Ident(String),
    Comment(String),
    ForLoop {
        initializer: UntypedAstNodeId,
        condition: UntypedAstNodeId,
        increment: UntypedAstNodeId,
        block: UntypedAstNodeId,
    },
    UnaryExpression {
        op: UnaryOp,
        expression: UntypedAstNodeId,
    },
    FunctionCall {
        callee: String,
        arguments: Vec<UntypedAstNodeId>,
    },
}

#[derive(Debug, Clone)]
pub struct Parameter {
    pub name: String,
    pub ty: TypeAnnotation,
}

#[derive(Debug, Clone)]
pub enum LiteralValue {
    RawString(String),
    Int(i64),
    Float(f64),
    Bool(bool),
}

pub type UntypedAstNodeId = usize;

#[derive(Debug, Clone)]
pub enum BinOp {
    Add,
    Sub,
    Multiply,
    Divide,
    Modulo,
    Or,
    And,
    NotEqual,
    Equal,
    LesserEqual,
    GreaterEqual,
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Negate,
    Not,
    PreIncrement,
    PreDecrement,
    PostIncrement,
    PostDecrement,
}

#[derive(Debug, Clone)]
pub struct TypeAnnotation {
    pub ty: String,
}

#[derive(Default)]
pub struct UntypedAstArena {
    nodes: Vec<UntypedAstNode>,
}

impl UntypedAstArena {
    pub fn alloc(&mut self, node: UntypedAstNode) -> UntypedAstNodeId {
        let id = self.nodes.len();
        self.nodes.push(node);
        id
    }

    pub fn get(&self, id: UntypedAstNodeId) -> &UntypedAstNode {
        &self.nodes[id]
    }

    pub fn nodes(&self) -> &[UntypedAstNode] {
        &self.nodes
    }
}
