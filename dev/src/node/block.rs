use crate::ast::AstContext;
use crate::node::{Node, NodeID, NodeIterator, NodeType, NodeUsage, Statement};
use crate::{Ast, Result, Variable};
use std::collections::HashMap;
use std::fmt::Debug;

#[derive(Debug, Clone)]
pub struct Block {
    pub variables: HashMap<String, NodeID<Variable>>,
    children: Vec<NodeID<Statement>>,
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
    fn node_type(_: NodeID<Self>, _: &Ast, _usage: NodeUsage) -> Result<NodeType> {
        Ok(NodeType::Void)
    }

    fn children(&self, _context: AstContext) -> NodeIterator<'_> {
        NodeIterator::slice(&self.children)
    }

    fn has_variable(&self, var: &str) -> Result<Option<NodeID<Variable>>> {
        if let Some(variable_id) = self.variables.get(var) {
            Ok(Some((*variable_id).into()))
        } else {
            Ok(None)
        }
    }
}
