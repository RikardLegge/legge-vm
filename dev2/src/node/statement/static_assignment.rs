use crate::ast::{AstNode, AstNodeRef};
use crate::children::{ChildIterator, Children};
use crate::node::{
    Ast, Break, EvaluateExpression, Expression, FunctionDeclaration, Loop, NodeID, Result, Return,
    StaticAssignment, TypeDeclaration, Variable, VariableValue,
};
use crate::state::State;
use crate::types::{NodeType, NodeUsage, Types};
use std::borrow::Cow;

pub struct StaticAssignmentStorage {
    pub assign_to: NodeID<VariableValue>,
    pub variable: NodeID<Variable>,
    pub value: NodeID<Expression>,
    pub is_associated_field: bool,
}

impl StaticAssignmentStorage {
    pub fn new(
        assign_to: NodeID<VariableValue>,
        variable: NodeID<Variable>,
        value: NodeID<Expression>,
    ) -> Self {
        Self {
            assign_to,
            variable,
            value,
            is_associated_field: false,
        }
    }
}

impl Types for AstNodeRef<StaticAssignment> {
    fn get_type<'this, 'ast>(
        &'this self,
        ast: &'ast Ast,
        usage: NodeUsage,
    ) -> Result<Cow<'ast, NodeType>> {
        match usage {
            NodeUsage::Type => {
                let node = ast.body(self.id);
                ast.get(node.value).get_type(ast, usage)
            }
            NodeUsage::Value => Ok(Cow::Owned(NodeType::Void)),
        }
    }
}

impl Children for AstNodeRef<StaticAssignment> {
    fn children<'this, 'ast>(&'this self, ast: &'ast Ast) -> ChildIterator<'ast> {
        let node = ast.body(self.id);
        ChildIterator::new(
            [
                node.assign_to.into(),
                node.variable.into(),
                node.value.into(),
            ]
            .into(),
        )
    }
}
