use crate::ast::{AstNode, AstNodeRef};
use crate::children::{ChildIterator, Children};
use crate::linker::Linker;
use crate::node::statement::ReturnStorage;
use crate::node::{
    Ast, Block, Error, Expression, Loop, NodeID, Result, Statement, TypeDeclaration, Variable,
};
use crate::state::State;
use crate::types::{NodeType, NodeUsage, Types};
use std::borrow::Cow;
use std::collections::HashMap;
use std::ops::Deref;

#[derive(Debug)]
pub struct LoopStorage {
    pub body: NodeID<Block>,
    pub value: Option<NodeType>,
}

impl LoopStorage {
    pub fn new(body: NodeID<Block>, value: Option<NodeType>) -> Self {
        Self { body, value }
    }
}

impl Types for AstNodeRef<Loop> {
    fn get_type<'this, 'ast>(
        &'this self,
        ast: &'ast Ast,
        _usage: NodeUsage,
    ) -> Result<Cow<'ast, NodeType>> {
        let node = ast.body(self.id);
        match node.value {
            Some(ref value) => Ok(Cow::Borrowed(value)),
            None => Err(Error::TypeNotInferred(self.id.into())),
        }
    }
}

impl Children for AstNodeRef<Loop> {
    fn children<'this, 'ast>(&'this self, ast: &'ast Ast) -> ChildIterator<'ast> {
        let node = ast.body(self.id);
        ChildIterator::new([node.body.into()].into())
    }
}

impl Linker for AstNodeRef<Loop> {}
