use crate::ast::{AstNode, AstNodeRef};
use crate::children::{ChildIterator, Children};
use crate::node::statement::ReturnStorage;
use crate::node::{Ast, Block, Expression, NodeID, Result, Statement, TypeDeclaration, Variable};
use crate::state::State;
use crate::types::{NodeType, NodeUsage, Types};
use std::borrow::Cow;
use std::collections::HashMap;
use std::ops::Deref;

pub struct BlockStorage {
    pub variables: HashMap<String, NodeID<Variable>>,
    pub children: Vec<NodeID<Statement>>,
}

impl BlockStorage {
    pub fn new(children: Vec<NodeID<Statement>>, ast: &Ast) -> Self {
        let variables = children
            .iter()
            .map(|id| ast.get(*id))
            .filter_map(|statement| statement.variable())
            .map(|id| (ast.body(id).name.to_string(), id))
            .collect();
        Self {
            variables,
            children,
        }
    }
}

impl AstNode<Block> {
    fn has_variable(&self, var: &str) -> Result<Option<NodeID<Variable>>> {
        if let Some(variable_id) = self.body().variables.get(var) {
            Ok(Some(*variable_id))
        } else {
            Ok(None)
        }
    }
}

impl Types for AstNodeRef<Block> {
    fn get_type<'this, 'ast>(
        &'this self,
        ast: &'ast Ast,
        usage: NodeUsage,
    ) -> Result<Cow<'ast, NodeType>> {
        match usage {
            NodeUsage::Type => panic!(),
            NodeUsage::Value => {
                let last = ast.body(self.id).children.last();
                match last {
                    None => Ok(Cow::Owned(NodeType::Void)),
                    Some(last) => ast.get(*last).get_type(ast, usage),
                }
            }
        }
    }
}

impl Children for AstNodeRef<Block> {
    fn children<'this, 'ast>(&'this self, ast: &'ast Ast) -> ChildIterator<'ast> {
        let node = ast.body(self.id);
        ChildIterator::new(([node.children.deref().into()].into()))
    }
}