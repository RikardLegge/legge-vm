use crate::ast::AstNodeRef;
use crate::children::{ChildIterator, Children};
use crate::node::{Ast, Expression, NodeID, Result, Variable, VariableDeclaration};
use crate::types::{NodeType, NodeUsage, Types};
use std::borrow::Cow;

#[derive(Debug)]
pub struct VariableDeclarationStorage {
    variable: NodeID<Variable>,
    value: NodeID<Expression>,
    is_const: bool,
}

impl VariableDeclarationStorage {
    pub fn new(variable: NodeID<Variable>, value: NodeID<Expression>, is_const: bool) -> Self {
        Self {
            variable,
            value,
            is_const,
        }
    }
}

impl Types for AstNodeRef<VariableDeclaration> {
    fn get_type<'this, 'ast>(
        &'this self,
        ast: &'ast Ast,
        usage: NodeUsage,
    ) -> Result<Cow<'ast, NodeType>> {
        match usage {
            NodeUsage::Type => {
                let value_id = ast.body(self.id).value;
                ast.get(value_id).get_type(ast, usage)
            }
            NodeUsage::Value => Ok(Cow::Owned(NodeType::Void)),
        }
    }
}

impl Children for AstNodeRef<VariableDeclaration> {
    fn children<'this, 'ast>(&'this self, ast: &'ast Ast) -> ChildIterator<'ast> {
        let node = ast.body(self.id);
        ChildIterator::new([node.variable.into(), node.value.into()].into())
    }
}
