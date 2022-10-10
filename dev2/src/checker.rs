use crate::ast::{AstNode, AstNodeRef, NodeBody};
use crate::node::{Ast, Result};

// Example of dynamically adding methods to the AST, which respect the node types
pub trait Checker {
    fn check(&self, ast: &Ast) -> Result<()>;
}

impl<Any: NodeBody> Checker for AstNode<Any>
where
    AstNodeRef<Any>: Checker,
{
    fn check(&self, ast: &Ast) -> Result<()> {
        self.get_ref().check(ast)
    }
}
