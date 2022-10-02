use crate::ast::AstContext;
use crate::node::{
    AstRootNode, FunctionDeclaration, NodeIterator, NodeState, Result, TypeDeclaration, Unknown,
};
use crate::{Ast, Error, NodeID, Variable};
use std::fmt::Debug;

#[derive(Debug, Clone)]
#[repr(C)]
pub struct AstNode<NodeType = Unknown> {
    pub id: NodeID<NodeType>,
    pub parent_id: Option<NodeID>,
    pub body: Option<AstRootNode>,
}

impl AstNode {
    pub fn link(node_id: NodeID, ast: &mut Ast, context: AstContext) -> Result<()> {
        AstRootNode::link(node_id.into(), ast, context)
    }

    pub fn node_type(node_id: NodeID, ast: &Ast, usage: NodeUsage) -> Result<NodeType> {
        AstRootNode::node_type(node_id.into(), ast, usage)
    }
}

impl<T> AstNode<T> {
    pub fn children(&self, context: AstContext) -> NodeIterator<'_> {
        self.body.as_ref().unwrap().children(context)
    }
}

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
    Indirect(NodeID<NodeState>),
    Custom(NodeID<TypeDeclaration>),
    Function(NodeID<FunctionDeclaration>),
}
