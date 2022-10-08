use crate::ast::{NodeBody, NodeID, NodeIterator, NodeUsage};
use crate::node::{get_node_type, Ast, AstContext, AstRootNode, NodeType, Statement, Variable};
use crate::{Error, Result};
use std::collections::HashMap;

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

impl NodeBody for Block {
    type Root = AstRootNode;
    type NodeType = NodeType;
    type AstContext = AstContext;
    type Variable = Variable;

    fn node_type(node_id: NodeID<Self>, ast: &Ast, usage: NodeUsage) -> Result<NodeType> {
        match usage {
            NodeUsage::Type => Err(Error::InternalError),
            NodeUsage::Call => Err(Error::InternalError),
            NodeUsage::Value => {
                let node = ast.get_typed(node_id);
                let last = node.body().children.last();
                match last {
                    None => Ok(NodeType::Void),
                    Some(last) => get_node_type(ast, *last, usage),
                }
            }
        }
    }

    fn children(&self, _context: AstContext) -> NodeIterator<'_, Self::AstContext> {
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
