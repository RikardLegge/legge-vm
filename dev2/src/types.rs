use crate::ast::{Ast, AstNode, AstNodeRef, NodeBody, NodeDataStorage};

// Example of dynamically adding methods to the AST, which respect the node types
pub trait Types<Any: NodeBody<Root = Any> + NodeDataStorage> {
    fn get_type(&self, ast: &Ast<Any::Root>);
}

impl<Any: NodeBody<Root = Any> + NodeDataStorage> Types<Any> for AstNode<Any>
where
    AstNodeRef<Any>: Types<Any>,
{
    fn get_type(&self, ast: &Ast<Any::Root>) {
        self.get_ref().get_type(ast)
    }
}
