use crate::node::{AstNodeBody, NodeType};
use crate::{Ast, Error, Result};
use crate::{Node, NodeID};

#[derive(Debug)]
pub struct Variable {
    pub name: String,
}

impl Variable {
    pub fn new(name: String) -> Self {
        Variable { name }
    }
}

impl Node for Variable {
    fn node_type(node_id: NodeID<Self>, ast: &Ast) -> Result<NodeType> {
        let node = ast.get(node_id);
        let parent_id = node.parent_id.ok_or(Error::InternalError)?;
        let parent = ast.get(parent_id);
        match parent.body.as_ref().unwrap() {
            AstNodeBody::Statement(statement) => statement.variable_type(ast),
            _ => unimplemented!(),
        }
    }
}
