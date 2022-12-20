use crate::ast::{AstNode, AstNodeRef};
use crate::children::{ChildIterator, Children};
use crate::linker::{LinkContext, Linker, LinkerExt};
use crate::node::{
    Ast, Break, Error, EvaluateExpression, Expression, FunctionDeclaration, Loop, NodeID, Result,
    Return, StaticAssignment, TypeDeclaration, Variable, VariableValue,
};
use crate::state::State;
use crate::types::{NodeType, NodeUsage, Types};
use std::borrow::Cow;

#[derive(Debug)]
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

impl Linker for AstNodeRef<StaticAssignment> {
    fn link(&self, ast: &mut Ast, context: LinkContext) -> Result<()> {
        let node = ast.body(self.id);
        let assign_to = ast.body(node.assign_to);
        if let State::Unlinked(variable_name) = &assign_to.variable {
            let variable_id = ast
                .closest_variable(self.id, variable_name, context)?
                .ok_or_else(|| Error::VariableNotFound(variable_name.into(), context))?;

            let body = ast.body_mut(node.assign_to);
            body.variable = State::Linked(variable_id);

            if let Some(type_id) = AstNode::type_declaration_id(variable_id, ast) {
                let body = ast.body_mut(self.id);
                body.is_associated_field = true;

                let body = ast.body(self.id);
                let variable = body.variable;
                let path = ast.body(variable).name.clone();

                let tp = ast.body_mut(type_id);
                tp.associated_values.insert(path, variable);
            }
        }
        Ok(())
    }
}
