use crate::ast::{AstNode, AstNodeRef};
use crate::children::{ChildIterator, Children};
use crate::node::statement::ReturnStorage;
use crate::node::{
    Ast, Block, Expression, ExpressionChain, If, Loop, NodeID, Result, Statement, TypeDeclaration,
    Variable,
};
use crate::state::State;
use crate::types::{NodeType, NodeUsage, Types};
use std::borrow::Cow;
use std::collections::HashMap;
use std::ops::Deref;

pub struct ExpressionChainStorage {
    pub lhs: NodeID<Expression>,
    pub rhs: NodeID<Expression>,
    linked: bool,
}

impl ExpressionChainStorage {
    pub fn new(lhs: NodeID<Expression>, rhs: NodeID<Expression>) -> Self {
        Self {
            lhs,
            rhs,
            linked: false,
        }
    }
}

impl Types for AstNodeRef<ExpressionChain> {
    fn get_type<'this, 'ast>(
        &'this self,
        ast: &'ast Ast,
        usage: NodeUsage,
    ) -> Result<Cow<'ast, NodeType>> {
        let node = ast.body(self.id);
        ast.get(node.rhs).get_type(ast, usage)
    }
}

impl Children for AstNodeRef<ExpressionChain> {
    fn children<'this, 'ast>(&'this self, ast: &'ast Ast) -> ChildIterator<'ast> {
        let node = ast.body(self.id);
        ChildIterator::new([node.lhs.into(), node.rhs.into()].into())
    }
}
