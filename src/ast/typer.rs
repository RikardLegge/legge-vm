use crate::ast::{Ast, NodeID, Result};
use std::collections::VecDeque;

pub fn infer_types(ast: &mut Ast) -> Result<()> {
    Typer::new(ast).infer_types()
}

pub struct Typer<'a> {
    queue: VecDeque<NodeID>,
    ast: &'a mut Ast,
}

impl<'a> Typer<'a> {
    pub fn new(ast: &'a mut Ast) -> Self {
        let queue = VecDeque::from(vec![ast.root()]);
        Self { queue, ast }
    }

    pub fn infer_types(mut self) -> Result<()> {
        while let Some(node_id) = self.queue.pop_front() {
            let node = self.ast.get_node(node_id);
            for &child_id in node.body.children() {
                if let None = self.ast.get_node(child_id).tp {
                    self.queue.push_back(child_id)
                }
            }
        }
        Ok(())
    }
}
