use crate::node::{Node, NodeID, NodeIterator, NodeType, Statement};
use crate::{Ast, Reference, Result};
use std::collections::HashMap;
use std::fmt::Debug;

#[derive(Debug)]
pub struct Block {
    pub variables: HashMap<String, NodeID<Reference>>,
    children: Vec<NodeID<Statement>>,
}

impl Block {
    pub fn new(children: Vec<NodeID<Statement>>, ast: &Ast) -> Self {
        let variables = children
            .iter()
            .map(|id| ast.get_inner(*id))
            .filter_map(|statement| statement.variable())
            .map(|id| (ast.get_inner(id).name.to_string(), id))
            .collect();
        Block {
            variables,
            children,
        }
    }
}

impl Node for Block {
    fn node_type(_: NodeID<Self>, _: &Ast) -> Result<NodeType> {
        Ok(NodeType::Void)
    }

    fn children(&self) -> NodeIterator<'_> {
        NodeIterator::slice(&self.children)
    }
}
