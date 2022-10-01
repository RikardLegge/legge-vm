use crate::node::NodeIDContext;
use crate::{Ast, AstNode, Error, Result};
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
    queue: VecDeque<NodeIDContext>,
}

impl Linker {
    pub fn link(mut self) -> Result<Ast> {
        while let Some(task) = self.queue.pop_front() {
            let NodeIDContext { node_id, context } = task;
            let node = self.ast.get(node_id);
            for child in node.children(context) {
                self.queue.push_back(child);
            }
            match AstNode::link(node_id, &mut self.ast, context) {
                Ok(_) => continue,
                Err(Error::TypeNotInferred) => self.queue.push_back(task),
                Err(err) => return Err(Error::AstError(self.ast, Box::new(err))),
            }
        }

        Ok(self.ast)
    }
}
