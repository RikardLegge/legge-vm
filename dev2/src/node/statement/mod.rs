mod evaluate_expression;
mod static_assignment;
mod stmt_break;
mod stmt_return;
mod type_declaration;
mod variable_assignment;
mod variable_declaration;

pub use evaluate_expression::*;
pub use static_assignment::*;
pub use stmt_break::*;
pub use stmt_return::*;
pub use type_declaration::*;
pub use variable_assignment::*;
pub use variable_declaration::*;

use crate::ast::{AstNode, AstNodeRef};
use crate::children::{ChildIterator, Children};
use crate::linker::{LinkContext, Linker};
use crate::node::{Ast, Expression, NodeID, Result, Statement, Storage, Variable};
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

impl Linker for AstNodeRef<Statement> {
    fn link(&self, ast: &mut Ast, context: LinkContext) -> Result<()> {
        let node = ast.get(self.id);
        reified! {node.link(ast, context)}
    }
}

impl Children for AstNodeRef<Statement> {
    fn children<'this, 'ast>(&'this self, ast: &'ast Ast) -> ChildIterator<'ast> {
        let node = ast.get(self.id);
        reified! {node.children(ast)}
    }
}

impl AstNode<Statement> {
    pub fn variable(&self) -> Option<NodeID<Variable>> {
        match self.storage() {
            Storage::VariableDeclaration(dec) => Some(dec.variable),
            Storage::VariableAssignment(_) => None,
            Storage::TypeDeclaration(dec) => Some(dec.variable),
            Storage::StaticAssignment(_) => None,
            Storage::EvaluateExpression(_) => None,
            Storage::Return(_) => None,
            Storage::Break(_) => None,
            _ => unimplemented!(),
        }
    }

    pub fn value(&self) -> Option<NodeID> {
        match self.storage() {
            Storage::VariableDeclaration(var) => Some(var.value.into()),
            Storage::VariableAssignment(var) => Some(var.value.into()),
            Storage::TypeDeclaration(var) => Some(var.constructor.into()),
            Storage::StaticAssignment(var) => Some(var.value.into()),
            Storage::EvaluateExpression(var) => Some(var.value.into()),
            Storage::Return(var) => var.value.map(Into::into),
            Storage::Break(var) => var.value.map(Into::into),
            _ => unimplemented!(),
        }
    }
}
