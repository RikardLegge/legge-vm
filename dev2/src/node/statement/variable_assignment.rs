use crate::ast::{AstNode, AstNodeRef};
use crate::children::{ChildIterator, Children};
use crate::node::{
    Ast, Break, EvaluateExpression, Expression, FunctionDeclaration, Loop, NodeID, Result, Return,
    TypeDeclaration, Variable, VariableAssignment,
};
use crate::state::State;
use crate::types::{NodeType, NodeUsage, Types};
use std::borrow::Cow;

pub struct VariableAssignmentStorage {
    pub variable: State<String, NodeID<Variable>>,
    pub value: NodeID<Expression>,
}

impl VariableAssignmentStorage {
    pub fn new(ident: String, value: NodeID<Expression>) -> Self {
        Self {
            variable: State::Unlinked(ident),
            value,
        }
    }
}

impl Types for AstNodeRef<VariableAssignment> {
    fn get_type<'this, 'ast>(
        &'this self,
        _ast: &'ast Ast,
        _usage: NodeUsage,
    ) -> Result<Cow<'ast, NodeType>> {
        Ok(Cow::Owned(NodeType::Void))
    }
}

impl Children for AstNodeRef<VariableAssignment> {
    fn children<'this, 'ast>(&'this self, ast: &'ast Ast) -> ChildIterator<'ast> {
        let node = ast.body(self.id);
        ChildIterator::new([node.value.into()].into())
    }
}
