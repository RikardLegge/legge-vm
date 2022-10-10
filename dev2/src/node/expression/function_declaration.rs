use crate::ast::AstNodeRef;
use crate::children::{ChildIterator, Children};
use crate::node::{Ast, Block, FunctionDeclaration, NodeID, Result, Variable};
use crate::types::{NodeType, NodeUsage, Types};
use std::borrow::Cow;
use std::ops::Deref;

#[derive(Debug)]
pub struct FunctionDeclarationStorage {
    pub arguments: Vec<NodeID<Variable>>,
    pub returns: NodeType,
    pub body: NodeID<Block>,
}

impl Types for AstNodeRef<FunctionDeclaration> {
    fn get_type<'this, 'ast>(
        &'this self,
        _ast: &'ast Ast,
        usage: NodeUsage,
    ) -> Result<Cow<'ast, NodeType>> {
        match usage {
            NodeUsage::Type | NodeUsage::Value => Ok(Cow::Owned(NodeType::Function(self.id))),
        }
    }
}

impl Children for AstNodeRef<FunctionDeclaration> {
    fn children<'this, 'ast>(&'this self, ast: &'ast Ast) -> ChildIterator<'ast> {
        let node = ast.body(self.id);
        ChildIterator::new([node.arguments.deref().into(), node.body.into()].into())
    }
}
