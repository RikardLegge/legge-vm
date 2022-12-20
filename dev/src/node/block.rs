use crate::ast::AstContext;
use crate::node::{Node, NodeID, NodeIterator, NodeType, NodeUsage, Statement};
use crate::{Ast, Error, Result, Variable};
use std::collections::HashMap;
use std::fmt::Debug;

#[derive(Debug, Clone)]
pub struct Block {
    pub variables: HashMap<String, NodeID<Variable>>,
    pub children: Vec<NodeID<Statement>>,
}

impl Block {
    pub fn new(children: Vec<NodeID<Statement>>, ast: &Ast) -> Self {
        let variables = children
            .iter()
            .map(|id| ast.get_body(*id))
            .filter_map(|statement| statement.variable())
            .map(|id| (ast.get_body(id).name.to_string(), id))
            .collect();
        Block {
            variables,
            children,
        }
    }
}

impl Node for Block {
    fn node_type(node_id: NodeID<Self>, ast: &Ast, usage: NodeUsage) -> Result<NodeType> {
        match usage {
            NodeUsage::Type => Err(Error::InternalError),
            NodeUsage::Call => Err(Error::InternalError),
            NodeUsage::Value => {
                let node = ast.get_typed(node_id);
                let last = node.body().children.last();
                match last {
                    None => Ok(NodeType::Void),
                    Some(last) => ast.get_node_type(*last, usage),
                }
            }
        }
    }

    fn children(&self, _context: AstContext) -> NodeIterator<'_> {
        NodeIterator::slice(&self.children)
    }

    fn has_variable(&self, var: &str) -> Result<Option<NodeID<Variable>>> {
        if let Some(variable_id) = self.variables.get(var) {
            Ok(Some(*variable_id))
        } else {
            Ok(None)
        }
    }
}
