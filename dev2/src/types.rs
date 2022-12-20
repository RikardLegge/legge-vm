use crate::ast::{Ast, AstNode, AstNodeRef, NodeBody, NodeDataStorage};

// Example of dynamically adding methods to the AST, which respect the node types
pub trait Types<Root: NodeBody<Root = Root> + NodeDataStorage> {
    fn get_type(&self, ast: &Ast<Root>);
}

impl<Any: NodeBody<Root = Root>, Root: NodeBody<Root = Root> + NodeDataStorage> Types<Root>
    for AstNode<Any>
where
    AstNodeRef<Any>: Types<Root>,
{
    fn get_type(&self, ast: &Ast<Root>) {
        self.get_ref().get_type(ast)
    }
}
