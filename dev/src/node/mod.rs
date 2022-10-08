mod expression;
mod statement;
mod variable;

pub use expression::*;
pub use statement::*;
pub use variable::*;

use crate::ast::{AnyNode, NodeBody, NodeID, NodeUsage};
use crate::{impl_node, impl_root_node, Error};

pub type Ast = crate::ast::Ast<AstRootNode>;
pub type AstNode<T = AnyNode> = crate::ast::AstNode<T, AstRootNode>;

#[derive(Debug, Default, Clone, Copy)]
pub enum AstContext {
    #[default]
    Default,
    Chain(NodeID),
}

pub fn closest_variable(
    ast: &Ast,
    node_id: impl Into<NodeID>,
    target_ident: &str,
    context: AstContext,
) -> crate::Result<Option<NodeID<Variable>>> {
    match context {
        AstContext::Default => ast.walk_up_closest::<Block, Variable>(node_id, |node| {
            node.body().has_variable(target_ident)
        }),
        AstContext::Chain(parent_id) => {
            let tp = get_node_type(ast, parent_id, NodeUsage::Type)?;
            match tp {
                NodeType::Custom(decl) => {
                    let body = ast.get_body(decl);
                    body.has_variable(target_ident)
                }
                _ => Ok(None),
            }
        }
    }
}

pub fn get_node_type(
    ast: &Ast,
    node_id: impl Into<NodeID>,
    usage: NodeUsage,
) -> crate::Result<NodeType> {
    match AstNode::node_type(node_id.into(), ast, usage)? {
        NodeType::Indirect(target_id) => get_node_type(ast, target_id, usage),
        tp => Ok(tp),
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
        Value,
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
