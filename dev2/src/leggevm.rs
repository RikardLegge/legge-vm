use crate::ast::AstNode;
use crate::node;
use crate::node::{Expression, Value};
use crate::types::Types;

pub fn run() {
    let mut ast = node::Ast::new();

    // Create a new node of type Value
    let id = ast.node(None, |_, _| node::ValueStorage());

    // Erase the type of the node handle
    // let id: node::NodeID = id.into();
    let node: &AstNode<Value> = ast.get(id);
    let node: &AstNode<Expression> = node.into();
    let node: &AstNode<Value> = node.try_into().unwrap();

    // Ensure that dynamic dispatch works as expected
    node.get_type(&ast);
}
