use crate::ast::{AstNode, AstNodeRef, NodeBody};
use crate::node::{Ast, NodeID, Result, Storage, Variable};
use crate::types::{NodeType, NodeUsage, Types};
use std::borrow::Borrow;
use std::convert::Infallible;

// Example of dynamically adding methods to the AST, which respect the node types
pub trait Linker {
    fn link(&self, _ast: &mut Ast, _context: LinkerContext) -> Result<()> {
        Ok(())
    }
}

impl<Any: NodeBody> Linker for AstNode<Any>
where
    AstNodeRef<Any>: Linker,
{
    fn link(&self, ast: &mut Ast, context: LinkerContext) -> Result<()> {
        self.get_ref().link(ast, context)
    }
}

#[derive(Debug, Default, Clone, Copy)]
pub enum LinkerContext {
    #[default]
    Ast,
    Node(NodeID),
}

pub trait LinkerExt {
    fn closest_variable(
        &self,
        node_id: impl Into<NodeID>,
        target_ident: &str,
        context: LinkerContext,
    ) -> Result<Option<NodeID<Variable>>>;
}

impl LinkerExt for Ast {
    fn closest_variable(
        &self,
        node_id: impl Into<NodeID>,
        target_ident: &str,
        context: LinkerContext,
    ) -> Result<Option<NodeID<Variable>>> {
        Ok(match context {
            LinkerContext::Ast => self
                .walk_up::<Variable, Infallible>(node_id, |node| {
                    Ok(match node.storage() {
                        Storage::Block(block) => block.variables.get(target_ident).cloned(),
                        _ => None,
                    })
                })
                .unwrap(),
            LinkerContext::Node(parent_id) => {
                let tp = self.get(parent_id).get_type(self, NodeUsage::Type)?;
                match tp.borrow() {
                    NodeType::Custom(decl) => {
                        let body = self.body(*decl);
                        body.associated_values.get(target_ident).cloned()
                    }
                    _ => None,
                }
            }
        })
    }
}
