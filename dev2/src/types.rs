use crate::ast::{AstNode, AstNodeRef, NodeBody};
use crate::node::{Ast, FunctionDeclaration, NodeID, Result, TypeDeclaration, VariableValue};
use std::borrow::Cow;

// Example of dynamically adding methods to the AST, which respect the node types
pub trait Types {
    fn get_type<'this, 'ast>(
        &'this self,
        ast: &'ast Ast,
        usage: NodeUsage,
    ) -> Result<Cow<'ast, NodeType>>;
}

impl<Any: NodeBody> Types for AstNode<Any>
where
    AstNodeRef<Any>: Types,
{
    fn get_type<'this, 'ast>(
        &'this self,
        ast: &'ast Ast,
        usage: NodeUsage,
    ) -> Result<Cow<'ast, NodeType>> {
        self.get_ref().get_type(ast, usage)
    }
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

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum NodeUsage {
    Type,
    Value,
}
