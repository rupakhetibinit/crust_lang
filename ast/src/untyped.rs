#[derive(Debug, Clone)]
pub enum UntypedAstNode<'u> {
    BinaryExpression {
        left: UntypedAstNodeId,
        op: BinOp,
        right: UntypedAstNodeId,
    },
    Literal(LiteralValue<'u>),
    Program(Vec<UntypedAstNodeId>),
    LetStatement {
        identifier: &'u str,
        ty: Option<TypeAnnotation<'u>>,
        value: UntypedAstNodeId,
    },
    ReturnStatement {
        value: UntypedAstNodeId,
    },
}

#[derive(Debug, Clone)]
pub enum LiteralValue<'u> {
    RawString(&'u str),
    Int(i64),
    Float(f64),
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
}

#[derive(Debug, Clone)]
pub struct TypeAnnotation<'u> {
    pub ty: &'u str,
}

#[derive(Default)]
pub struct UntypedAstArena<'arena> {
    nodes: Vec<UntypedAstNode<'arena>>,
}

impl<'arena> UntypedAstArena<'arena> {
    pub fn alloc(&mut self, node: UntypedAstNode<'arena>) -> UntypedAstNodeId {
        let id = self.nodes.len();
        self.nodes.push(node);
        id
    }

    pub fn get(&self, id: UntypedAstNodeId) -> &UntypedAstNode<'arena> {
        &self.nodes[id]
    }

    pub fn nodes(&self) -> &[UntypedAstNode<'arena>] {
        &self.nodes
    }
}
