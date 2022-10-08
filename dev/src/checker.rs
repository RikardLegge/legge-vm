use crate::ast::{AnyNode, AstNode, NodeIDContext};
use crate::node::{Ast, AstContext, AstRootNode};
use crate::Result;
use std::collections::VecDeque;

pub fn check(ast: Ast) -> Result<Ast> {
    let root = ast.root.unwrap();
    Checker {
        ast,
        queue: VecDeque::from([root.into()]),
    }
    .check()
}

pub struct Checker {
    ast: Ast,
    queue: VecDeque<NodeIDContext<AstContext>>,
}

impl Checker {
    pub fn check(mut self) -> Result<Ast> {
        while let Some(task) = self.queue.pop_front() {
            let NodeIDContext { node_id, context } = task;
            let node = self.ast.get(node_id);
            for child in node.children(context) {
                self.queue.push_back(child);
            }
            match AstNode::<AnyNode, AstRootNode>::check(node_id, &mut self.ast) {
                Ok(_) => continue,
                Err(err) => unimplemented!("{:?}", err),
            }
        }

        Ok(self.ast)
    }
}
