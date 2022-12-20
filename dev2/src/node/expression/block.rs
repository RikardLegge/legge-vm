use crate::ast::{AstNode, AstNodeRef, NodeBody};
use crate::children::{ChildIterator, Children};
use crate::linker::Linker;
use crate::node::{
    Any, Ast, Block, Error, NodeID, Result, Statement, Storage, TypeDeclaration, Variable,
};
use crate::types::{NodeType, NodeUsage, Types};
use std::borrow::Cow;
use std::collections::HashMap;
use std::ops::Deref;

impl From<()> for Storage {
    fn from(_: ()) -> Self {
        Storage::Any(())
    }
}

#[derive(Debug)]
pub struct BlockStorage {
    pub variables: HashMap<String, NodeID<Variable>>,
    pub children: Vec<NodeID<dyn Statement>>,
}

impl BlockStorage {
    pub fn new(children: Vec<NodeID<dyn Statement>>, ast: &Ast) -> Self {
        let variables = Self::variables(&children, ast);
        let mut block = Self {
            variables: Default::default(),
            children: Default::default(),
        };
        block.set(children, variables);
        block
    }

    pub fn variables(
        children: &[NodeID<Statement>],
        ast: &Ast,
    ) -> HashMap<String, NodeID<Variable>> {
        children
            .iter()
            .map(|id| ast.get(*id))
            .filter_map(|statement| statement.variable())
            .map(|id| (ast.body(id).name.to_string(), id))
            .collect()
    }

    pub fn set(
        &mut self,
        children: Vec<NodeID<Statement>>,
        variables: HashMap<String, NodeID<Variable>>,
    ) {
        self.variables = variables;
        self.children = children;
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
            NodeUsage::Type | NodeUsage::Value => {
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

impl Linker for AstNodeRef<Block> {}
