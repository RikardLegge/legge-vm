mod ast_node;
mod block;
mod expression;
mod iterator;
mod r#loop;
mod macros;
mod node_id;
mod statement;
mod variable;

pub use ast_node::{Node, NodeType, NodeUsage};
pub use iterator::NodeIterator;

pub use block::*;
pub use expression::*;
pub use node_id::*;
pub use r#loop::*;
pub use statement::*;
pub use variable::*;

use crate::ast::AstContext;
use crate::{impl_node, impl_root_node, Ast, Error, Result};
use once_cell::unsync::OnceCell;
use std::fmt::{Debug, Formatter};

#[derive(Debug, Copy, Clone)]
pub struct Unknown();

#[derive(Debug, Clone)]
struct NodeTypeUsages {
    tp: OnceCell<NodeType>,
    call: OnceCell<NodeType>,
    value: OnceCell<NodeType>,
}

#[derive(Debug, Clone)]
#[repr(C)]
pub struct AstNode<Type = Unknown> {
    pub id: NodeID<Type>,
    pub parent_id: Option<NodeID>,
    node_type: NodeTypeUsages,
    body: Option<AstRootNode>,
}

impl AstNode {
    pub fn new(id: impl Into<NodeID>, parent_id: Option<impl Into<NodeID>>) -> AstNode {
        AstNode {
            id: id.into(),
            parent_id: parent_id.map(|id| id.into()),
            node_type: NodeTypeUsages {
                tp: OnceCell::new(),
                call: OnceCell::new(),
                value: OnceCell::new(),
            },
            body: None,
        }
    }

    pub fn check(node_id: NodeID, ast: &mut Ast) -> Result<()> {
        AstRootNode::check(node_id.into(), ast)
    }

    pub fn link(node_id: NodeID, ast: &mut Ast, context: AstContext) -> Result<()> {
        AstRootNode::link(node_id.into(), ast, context)
    }

    pub fn node_type(node_id: NodeID, ast: &Ast, usage: NodeUsage) -> Result<NodeType> {
        AstRootNode::node_type(node_id.into(), ast, usage)
    }

    pub fn body(&self) -> &Option<AstRootNode> {
        &self.body
    }

    pub fn body_mut(&mut self) -> &mut Option<AstRootNode> {
        &mut self.body
    }
}

impl<T> AstNode<T> {
    pub fn children(&self, context: AstContext) -> NodeIterator<'_> {
        match self.body.as_ref() {
            None => NodeIterator::empty(),
            Some(body) => body.children(context),
        }
    }
}

impl_root_node!(
    pub struct AstRootNode(AstNodeBody)
);

impl_node!(
    pub enum AstRootNode => AstNodeBody {
        Statement,
        Expression,
        Variable,
    }
);

impl_node!(
    pub enum AstRootNode => Expression {
        ConstValue,
        VariableValue,
        Operation,
        FunctionCall,
        ExpressionChain,
        Loop,
        If,
        Block,
        FunctionDeclaration,
    }
);

impl_node!(
    pub enum AstRootNode => Statement {
        VariableDeclaration,
        VariableAssignment,
        StaticAssignment,
        TypeDeclaration,
        EvaluateExpression,
        Return,
        Break,
    }
);
