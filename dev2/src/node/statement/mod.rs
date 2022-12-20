mod stmt_break;
mod stmt_return;
mod type_declaration;
mod variable_declaration;

pub use stmt_break::*;
pub use stmt_return::*;
pub use type_declaration::*;
pub use variable_declaration::*;

use crate::ast::{AstNode, AstNodeRef};
use crate::node::{Ast, NodeID, Result, Statement, Storage, Variable};
use crate::reified;
use crate::types::{NodeType, NodeUsage, Types};
use std::borrow::Cow;

impl Types for AstNodeRef<Statement> {
    fn get_type<'this, 'ast>(
        &'this self,
        ast: &'ast Ast,
        usage: NodeUsage,
    ) -> Result<Cow<'ast, NodeType>> {
        let node = ast.get(self.id);
        reified! {node.get_type(ast, usage)}
    }
}

impl AstNode<Statement> {
    pub fn variable(&self) -> Option<NodeID<Variable>> {
        match self.storage() {
            // Statement::VariableDeclaration(dec) => Some(dec.variable),
            // Statement::VariableAssignment(_) => None,
            // Statement::TypeDeclaration(dec) => Some(dec.variable),
            // Statement::StaticAssignment(_) => None,
            // Statement::EvaluateExpression(_) => None,
            // Statement::Return(_) => None,
            // Statement::Break(_) => None,
            _ => unimplemented!(),
        }
    }

    pub fn value(&self) -> Option<NodeID> {
        match self.storage() {
            // Statement::VariableDeclaration(var) => Some(var.value.into()),
            // Statement::VariableAssignment(var) => Some(var.value.into()),
            // Statement::TypeDeclaration(var) => Some(var.constructor.into()),
            // Statement::StaticAssignment(var) => Some(var.value.into()),
            // Statement::EvaluateExpression(var) => Some(var.value.into()),
            // Statement::Return(var) => var.value.map(Into::into),
            // Statement::Break(var) => var.value.map(Into::into),
            _ => unimplemented!(),
        }
    }
}
