use super::{Ast, NodeBody, NodeID, Result};
use crate::ast::ast::NodeReference;
use crate::ast::NodeReferenceType;
use std::collections::VecDeque;
use std::mem;

pub fn link(ast: &mut Ast) -> Result<()> {
    Linker::new(ast).link()
}

struct Linker<'a> {
    queue: VecDeque<NodeID>,
    ast: &'a mut Ast,
}

impl<'a> Linker<'a> {
    fn new(ast: &'a mut Ast) -> Self {
        let queue = VecDeque::from(vec![ast.root()]);
        Self { queue, ast }
    }

    fn add_ref(&mut self, target_id: NodeID, referencer: NodeID, tp: NodeReferenceType) {
        let node_ref = NodeReference::new(referencer, tp);
        self.ast
            .get_node_mut(target_id)
            .referenced_by
            .insert(node_ref);
    }

    fn link(mut self) -> Result<()> {
        while let Some(node_id) = self.queue.pop_front() {
            let node = self.ast.get_node(node_id);
            for &child in node.body.children() {
                self.queue.push_back(child);
            }
            match &node.body {
                NodeBody::Unlinked(body) => {
                    use super::NodeReferenceType::*;
                    use super::UnlinkedNodeBody::*;
                    let new_body = match body {
                        VariableAssignment(ident, expr_id) => {
                            let expr_id = *expr_id;
                            let target_id = self.ast.closest_variable(node_id, &ident)?;
                            self.add_ref(target_id, node_id, ReceiveValue);
                            NodeBody::VariableAssignment(target_id, expr_id)
                        }
                        VariableValue(ident) => {
                            let target_id = self.ast.closest_variable(node_id, &ident)?;
                            self.add_ref(target_id, node_id, AssignValue);
                            NodeBody::VariableValue(target_id)
                        }
                        Return => {
                            let target_id = self.ast.closest_fn(node_id)?;
                            self.add_ref(target_id, node_id, GoTo);
                            NodeBody::Return(target_id)
                        }
                        Break => {
                            let target_id = self.ast.closest_loop(node_id)?;
                            self.add_ref(target_id, node_id, GoTo);
                            NodeBody::Break(target_id)
                        }
                        Call(ident, _) => {
                            let target_id = self.ast.closest_variable(node_id, &ident)?;
                            // We move the args out the old NodeBody so that we do not have to copy
                            // the argument vec.
                            let node = self.ast.get_node_mut(node_id);
                            let args = match &mut node.body {
                                NodeBody::Unlinked(Call(_, args)) => mem::replace(args, Vec::new()),
                                _ => unreachable!(),
                            };
                            self.add_ref(target_id, node_id, GoTo);
                            NodeBody::Call(target_id, args)
                        }
                    };
                    let node = self.ast.get_node_mut(node_id);
                    node.body = new_body;
                }
                _ => (),
            }
        }
        Ok(())
    }
}
