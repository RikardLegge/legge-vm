use crate::ast::{AstNode, AstNodeRef};
use crate::children::{ChildIterator, Children};
use crate::node::{
    Ast, Break, Expression, FunctionDeclaration, Loop, NodeID, Result, Return, TypeDeclaration,
    Variable,
};
use crate::state::State;
use crate::types::{NodeType, NodeUsage, Types};
use std::borrow::Cow;

pub struct BreakStorage {
    pub r#loop: State<(), NodeID<Loop>>,
    pub value: Option<NodeID<Expression>>,
}

impl BreakStorage {
    pub fn new(value: Option<NodeID<Expression>>) -> Self {
        Self {
            r#loop: ().into(),
            value,
        }
    }
}

impl Types for AstNodeRef<Break> {
    fn get_type<'this, 'ast>(
        &'this self,
        _ast: &'ast Ast,
        _usage: NodeUsage,
    ) -> Result<Cow<'ast, NodeType>> {
        Ok(Cow::Owned(NodeType::Void))
    }
}

impl Children for AstNodeRef<Break> {
    fn children<'this, 'ast>(&'this self, ast: &'ast Ast) -> ChildIterator<'ast> {
        let node = ast.body(self.id);
        match node.value {
            Some(value) => ChildIterator::new(([value.into()].into())),
            None => ChildIterator::new([].into()),
        }
    }
}