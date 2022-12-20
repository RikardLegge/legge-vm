use crate::ast::{AstNode, AstNodeRef};
use crate::children::{ChildIterator, Children};
use crate::linker::Linker;
use crate::node::statement::ReturnStorage;
use crate::node::{
    Ast, Block, Expression, NodeID, Result, Statement, TypeDeclaration, Value, Variable,
};
use crate::state::State;
use crate::types::{NodeType, NodeUsage, Types};
use std::borrow::Cow;
use std::collections::HashMap;
use std::ops::Deref;

#[derive(Debug)]
pub enum ValueStorage {
    Int(isize),
    Float(f64),
    String(String),
    Custom(NodeID<TypeDeclaration>),
}

impl Types for AstNodeRef<Value> {
    fn get_type<'this, 'ast>(
        &'this self,
        ast: &'ast Ast,
        usage: NodeUsage,
    ) -> Result<Cow<'ast, NodeType>> {
        Ok(Cow::Owned(match ast.body(self.id) {
            ValueStorage::Int(_) => NodeType::Int,
            ValueStorage::Float(_) => NodeType::Float,
            ValueStorage::String(_) => NodeType::String,
            ValueStorage::Custom(id) => NodeType::Custom(*id),
        }))
    }
}

impl Children for AstNodeRef<Value> {
    fn children<'this, 'ast>(&'this self, ast: &'ast Ast) -> ChildIterator<'ast> {
        ChildIterator::new([].into())
    }
}

impl Linker for AstNodeRef<Value> {}
