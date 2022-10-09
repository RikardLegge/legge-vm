use crate::ast::{AstNode, AstNodeRef, NodeBody};
use crate::node::{Ast, NodeID, Result};

// Example of dynamically adding methods to the AST, which respect the node types
pub trait Linker {
    fn link(&self, ast: &mut Ast, context: LinkerContext) -> Result<()>;
}

impl<Any: NodeBody> Linker for AstNode<Any>
where
    AstNodeRef<Any>: Linker,
{
    fn link(&self, ast: &mut Ast, context: LinkerContext) -> Result<()> {
        self.get_ref().link(ast, context)
    }
}

#[derive(Debug, Default, Clone, Copy)]
pub enum LinkerContext {
    #[default]
    Ast,
    Node(NodeID),
}
