use crate::ast::AstNode;
use crate::children::Children;
use crate::node;
use crate::node::{Any, Variable};
use crate::types::Types;

pub fn run() {
    let mut ast = node::Ast::new();

    // Create a new node of type Value
    let id = ast.node_body(None, node::VariableStorage::new("test".into()));

    // Erase the type of the node handle
    // let id: node::NodeID = id.into();
    let node: &AstNode<Variable> = ast.get(id);
    let node: &AstNode<Any> = node.into();
    let node: &AstNode<Variable> = node.try_into().unwrap();

    // Ensure that dynamic dispatch works as expected
    // node.get_type(&ast, NodeUsage::Value);
    for child in node.children(&ast) {
        println!("{:?}", child);
    }
}
