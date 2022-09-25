use crate::{Ast, AstNode, Error, NodeID, Result};
use std::collections::VecDeque;

pub fn link_ast(ast: Ast) -> Result<Ast> {
    let root = ast.root.unwrap();
    Linker {
        ast,
        queue: VecDeque::from([root.into()]),
    }
    .link()
}

pub struct Linker {
    ast: Ast,
    queue: VecDeque<NodeID>,
}

impl Linker {
    pub fn link(mut self) -> Result<Ast> {
        while let Some(node_id) = self.queue.pop_front() {
            let node = self.ast.get(node_id);
            for &child in node.children() {
                self.queue.push_back(child);
            }
            match AstNode::link(node_id, &mut self.ast) {
                Ok(_) => continue,
                Err(Error::TypeNotInferred) => self.queue.push_back(node_id),
                Err(err) => return Err(err),
            }
        }

        Ok(self.ast)
    }
}
