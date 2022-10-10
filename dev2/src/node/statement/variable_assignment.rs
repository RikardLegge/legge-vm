use crate::ast::{AstNode, AstNodeRef};
use crate::children::{ChildIterator, Children};
use crate::linker::{Linker, LinkerContext, LinkerExt};
use crate::node::{
    Ast, Break, Error, EvaluateExpression, Expression, FunctionDeclaration, Loop, NodeID, Result,
    Return, StaticAssignment, TypeDeclaration, Variable, VariableAssignment,
};
use crate::state::State;
use crate::types::{NodeType, NodeUsage, Types};
use std::borrow::Cow;

#[derive(Debug)]
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

impl Linker for AstNodeRef<VariableAssignment> {
    fn link(&self, ast: &mut Ast, context: LinkerContext) -> Result<()> {
        let node = ast.body(self.id);
        if let State::Unlinked(var) = &node.variable {
            let var = ast
                .closest_variable(self.id, var, context)?
                .ok_or_else(|| Error::VariableNotFound(var.into()))?;

            let node = ast.body_mut(self.id);
            node.variable = State::Linked(var);
        }
        Ok(())
    }
}
