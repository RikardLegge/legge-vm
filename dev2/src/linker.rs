use crate::ast::{AstNode, AstNodeRef, NodeBody};
use crate::children::Children;
use crate::node::{Any, Ast, Error, NodeID, Result, Storage, Variable};
use crate::types::{NodeType, NodeUsage, Types};
use std::borrow::Borrow;
use std::collections::VecDeque;
use std::convert::Infallible;

pub fn link(mut ast: Ast, root: impl Into<NodeID>) -> Result<Ast>
where
    AstNodeRef<Any>: Children,
{
    let mut queue = VecDeque::from([(root.into(), LinkContext::Ast, true)]);
    let mut last_progress = 0;
    while let Some((node_id, ctx, first_visit)) = queue.pop_front() {
        let node = ast.get(node_id);

        if first_visit {
            for child in node.children(&ast) {
                let child_ctx = node.link_context(&ast, child, ctx)?;
                queue.push_back((child, child_ctx, true));
            }
        }

        let node_ref = node.get_ref();
        match node_ref.link(&mut ast, ctx) {
            Ok(_) => {
                last_progress = 0;
                continue;
            }
            Err(err @ Error::TypeNotInferred(_) | err @ Error::UnlinkedNode(_)) => {
                if last_progress > queue.len() {
                    return Err(Error::FatalError(ast, Box::new(err)));
                }
                last_progress += 1;
                queue.push_back((node_id, ctx, false))
            }
            Err(err) => return Err(Error::FatalError(ast, Box::new(err))),
        }
    }

    Ok(ast)
}

// Example of dynamically adding methods to the AST, which respect the node types
pub trait Linker {
    fn link(&self, _ast: &mut Ast, _context: LinkContext) -> Result<()> {
        Ok(())
    }

    fn link_context(
        &self,
        _ast: &Ast,
        child_id: impl Into<NodeID>,
        context: LinkContext,
    ) -> Result<LinkContext> {
        Ok(context)
    }
}

impl<Any: NodeBody> Linker for AstNode<Any>
where
    AstNodeRef<Any>: Linker,
{
    fn link(&self, ast: &mut Ast, context: LinkContext) -> Result<()> {
        self.get_ref().link(ast, context)
    }

    fn link_context(
        &self,
        ast: &Ast,
        child_id: impl Into<NodeID>,
        context: LinkContext,
    ) -> Result<LinkContext> {
        self.get_ref().link_context(ast, child_id, context)
    }
}

#[derive(Debug, Default, Clone, Copy)]
pub enum LinkContext {
    #[default]
    Ast,
    Node(NodeID),
}

pub trait LinkerExt {
    fn closest_variable(
        &self,
        node_id: impl Into<NodeID>,
        target_ident: &str,
        context: LinkContext,
    ) -> Result<Option<NodeID<Variable>>>;
}

impl LinkerExt for Ast {
    fn closest_variable(
        &self,
        node_id: impl Into<NodeID>,
        target_ident: &str,
        context: LinkContext,
    ) -> Result<Option<NodeID<Variable>>> {
        Ok(match context {
            LinkContext::Ast => self
                .walk_up::<Variable, Infallible>(node_id, |node| {
                    Ok(match node.storage() {
                        Storage::Block(block) => block.variables.get(target_ident).cloned(),
                        _ => None,
                    })
                })
                .unwrap(),
            LinkContext::Node(parent_id) => {
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
