#![feature(associated_type_defaults)]

use crate::node::AstTypes;

mod ast;
mod macros;
mod node;

fn main() {
    let mut ast = node::Ast::new();
    let id = ast.node(None, |_| node::ValueData());
    // Ensure that dynamic dispatching works
    let id: node::NodeID = id.into();
    ast.get_type(id);
}
