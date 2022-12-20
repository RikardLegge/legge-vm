use crate::ast::{AstNode, AstNodeRef, NodeBody};
use crate::children::Children;
use crate::node::{Ast, Error, NodeID, Result};
use std::collections::VecDeque;

pub fn check(mut ast: Ast, root: impl Into<NodeID>) -> Result<Ast> {
    unimplemented!()
    // let mut queue = VecDeque::from([root.into()]);
    // while let Some(node_id) = queue.pop_front() {
    //     let node = ast.get(node_id);
    //     for child in node.children(&ast) {
    //         queue.push_back(child);
    //     }
    //
    //     match node.check(&mut ast) {
    //         Ok(_) => continue,
    //         Err(Error::TypeNotInferred(_)) | Err(Error::UnlinkedNode(_)) => {
    //             queue.push_back(node_id)
    //         }
    //         Err(err) => return Err(Error::AstError(ast, Box::new(err))),
    //     }
    // }
    //
    // Ok(ast)
}

// Example of dynamically adding methods to the AST, which respect the node types
pub trait Checker {
    fn check(&self, ast: &Ast) -> Result<()>;
}

impl<Any: NodeBody> Checker for AstNode<Any>
where
    AstNodeRef<Any>: Checker,
{
    fn check(&self, ast: &Ast) -> Result<()> {
        self.get_ref().check(ast)
    }
}
