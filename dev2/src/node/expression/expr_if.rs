use crate::ast::{AstNode, AstNodeRef};
use crate::children::{ChildIterator, Children};
use crate::node::statement::ReturnStorage;
use crate::node::{
    Ast, Block, Expression, If, Loop, NodeID, Result, Statement, TypeDeclaration, Variable,
};
use crate::state::State;
use crate::types::{NodeType, NodeUsage, Types};
use std::borrow::Cow;
use std::collections::HashMap;
use std::ops::Deref;

#[derive(Debug)]
pub struct IfStorage {
    pub cond: NodeID<Expression>,
    pub body: NodeID<Block>,
    pub r#else: Option<NodeID<Expression>>,
}

impl IfStorage {
    pub fn new(
        cond: NodeID<Expression>,
        body: NodeID<Block>,
        r#else: Option<NodeID<Expression>>,
    ) -> Self {
        Self { cond, body, r#else }
    }
}

impl Types for AstNodeRef<If> {
    fn get_type<'this, 'ast>(
        &'this self,
        ast: &'ast Ast,
        usage: NodeUsage,
    ) -> Result<Cow<'ast, NodeType>> {
        let node = ast.body(self.id);
        ast.get(node.body).get_type(ast, usage)
    }
}

impl Children for AstNodeRef<If> {
    fn children<'this, 'ast>(&'this self, ast: &'ast Ast) -> ChildIterator<'ast> {
        let node = ast.body(self.id);
        match node.r#else {
            Some(r#else) => {
                ChildIterator::new([node.cond.into(), node.body.into(), r#else.into()].into())
            }
            None => ChildIterator::new([node.cond.into(), node.body.into()].into()),
        }
    }
}
