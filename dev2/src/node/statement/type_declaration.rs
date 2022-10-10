use crate::ast::{AstNode, AstNodeRef};
use crate::children::{ChildIterator, Children};
use crate::node::{Ast, Expression, NodeID, Result, TypeDeclaration, Variable};
use crate::types::{NodeType, NodeUsage, Types};
use std::borrow::Cow;
use std::collections::HashMap;
use std::ops::Deref;

#[derive(Debug)]
pub struct TypeDeclarationStorage {
    pub variable: NodeID<Variable>,
    pub constructor: NodeID<Expression>,
    pub fields: Vec<NodeID<Variable>>,
    pub associated_values: HashMap<String, NodeID<Variable>>,
}

impl TypeDeclarationStorage {
    pub fn new(variable: NodeID<Variable>, constructor: NodeID<Expression>) -> Self {
        Self {
            variable,
            constructor,
            fields: Vec::new(),
            associated_values: HashMap::new(),
        }
    }
}

impl AstNode<TypeDeclaration> {
    fn has_variable(&self, var: &str) -> Result<Option<NodeID<Variable>>> {
        if let Some(variable_id) = self.body().associated_values.get(var) {
            Ok(Some(*variable_id))
        } else {
            Ok(None)
        }
    }
}

impl Types for AstNodeRef<TypeDeclaration> {
    fn get_type<'this, 'ast>(
        &'this self,
        _ast: &'ast Ast,
        usage: NodeUsage,
    ) -> Result<Cow<'ast, NodeType>> {
        match usage {
            NodeUsage::Type => Ok(Cow::Owned(NodeType::Custom(self.id))),
            NodeUsage::Value => Ok(Cow::Owned(NodeType::Void)),
        }
    }
}

impl Children for AstNodeRef<TypeDeclaration> {
    fn children<'this, 'ast>(&'this self, ast: &'ast Ast) -> ChildIterator<'ast> {
        let node = ast.body(self.id);
        ChildIterator::new(
            [
                node.variable.into(),
                node.constructor.into(),
                node.fields.deref().into(),
            ]
            .into(),
        )
    }
}
