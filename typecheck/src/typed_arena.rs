use crate::typed::{TypedAstNode, TypedAstNodeId};

#[derive(Default)]
pub struct TypedAstArena {
    nodes: Vec<TypedAstNode>,
}

impl TypedAstArena {
    pub fn alloc(&mut self, node: TypedAstNode) -> TypedAstNodeId {
        let id = self.nodes.len();
        self.nodes.push(node);
        id
    }

    pub fn get(&self, id: TypedAstNodeId) -> &TypedAstNode {
        &self.nodes[id]
    }

    pub fn nodes(&self) -> &[TypedAstNode] {
        &self.nodes
    }
}
