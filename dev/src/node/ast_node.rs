use crate::ast::AstContext;
use crate::node::{FunctionDeclaration, NodeIterator, Result, TypeDeclaration, VariableValue};
use crate::{Ast, Error, NodeID, Variable};
use std::fmt::Debug;

pub trait Node: Sized + Debug {
    fn node_type(_node_id: NodeID<Self>, _ast: &Ast, _usage: NodeUsage) -> Result<NodeType> {
        Err(Error::TypeNotInferred)
    }

    fn children(&self, _context: AstContext) -> NodeIterator<'_> {
        NodeIterator::empty()
    }

    fn link(_node_id: NodeID<Self>, _ast: &mut Ast, _context: AstContext) -> Result<()> {
        Ok(())
    }

    fn has_variable(&self, _var: &str) -> Result<Option<NodeID<Variable>>> {
        Ok(None)
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum NodeUsage {
    Type,
    Call,
    Value,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum NodeType {
    Void,
    Int,
    Float,
    String,
    Boolean,
    Indirect(NodeID<VariableValue>),
    Custom(NodeID<TypeDeclaration>),
    Function(NodeID<FunctionDeclaration>),
}
