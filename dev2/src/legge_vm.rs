use crate::ast::AstNode;
use crate::node;
use crate::node::plugins::Types;
use crate::node::Any;

pub fn run() {
    let mut ast = node::Ast::new();

    // Create a new node of type Value
    let id = ast.node(None, |_| node::ValueStorage());

    // Erase the type of the node handle
    let id: node::NodeID = id.into();
    let node: &AstNode<Any> = ast.get(id);

    // Ensure that dynamic dispatch works as expected
    node.get_type(&ast);
}
