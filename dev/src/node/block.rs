use crate::node::{Node, NodeID, NodeType, Statement};
use crate::{Ast, Result, Variable};
use std::collections::HashMap;
use std::fmt::Debug;

#[derive(Debug)]
pub struct Block {
    pub variables: HashMap<String, NodeID<Variable>>,
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

    fn children(&self) -> Box<dyn Iterator<Item = &NodeID> + '_> {
        Box::new(self.children.iter().map(|c| c.into()))
    }
}
