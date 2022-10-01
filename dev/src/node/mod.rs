mod ast_node;
mod block;
mod expression;
mod iterator;
mod macros;
mod node_id;
mod statement;
mod variable;

pub use ast_node::{AstNode, Node, NodeType, NodeUsage};
pub use iterator::NodeIterator;

pub use block::*;
pub use expression::*;
pub use node_id::*;
pub use statement::*;
pub use variable::*;

use crate::ast::AstContext;
use crate::{impl_node, impl_root_node, Ast, Error, Result};
use std::fmt::Debug;

#[derive(Debug, Copy, Clone)]
pub struct Unknown();

impl_root_node!(
    pub struct AstRootNode(AstNodeBody)
);

impl_node!(
    pub enum AstRootNode => AstNodeBody {
        Block,
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
    }
);
