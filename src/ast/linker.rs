use super::{Ast, NodeBody, NodeID, Result};
use crate::ast::ast::{NodeReference, NodeReferenceLocation};
use crate::ast::{NodeReferenceType, NodeValue};
use crate::runtime::Runtime;
use std::collections::VecDeque;
use std::mem;

pub fn link(ast: &mut Ast, runtime: &Runtime) -> Result<()> {
    Linker::new(ast, runtime).link()
}

struct Linker<'a, 'b> {
    queue: VecDeque<NodeID>,
    ast: &'a mut Ast,
    runtime: &'b Runtime,
}

impl<'a, 'b> Linker<'a, 'b> {
    fn new(ast: &'a mut Ast, runtime: &'b Runtime) -> Self {
        let queue = VecDeque::from(vec![ast.root()]);
        Self {
            queue,
            ast,
            runtime,
        }
    }

    fn add_ref(
        &mut self,
        target_id: NodeID,
        referencer: NodeID,
        tp: NodeReferenceType,
        loc: NodeReferenceLocation,
    ) {
        let node_ref = NodeReference::new(referencer, tp, loc);
        self.ast
            .get_node_mut(target_id)
            .referenced_by
            .insert(node_ref);
    }

    fn closest_variable(
        &self,
        node_id: NodeID,
        ident: &str,
    ) -> Result<Option<(NodeID, NodeReferenceLocation)>> {
        self.ast.closest_variable(node_id, ident)
    }

    fn closest_fn(&mut self, node_id: NodeID) -> Result<(NodeID, NodeReferenceLocation)> {
        match self.ast.closest_fn(node_id) {
            Some(id) => Ok(id),
            _ => Err(self.ast.error(
                "No function ancestor found starting at",
                "statement outside of function",
                vec![node_id],
            )),
        }
    }

    fn closest_loop(&mut self, node_id: NodeID) -> Result<(NodeID, NodeReferenceLocation)> {
        match self.ast.closest_loop(node_id) {
            Some(id) => Ok(id),
            _ => Err(self.ast.error(
                "No loop ancestor found starting at",
                "statement outside of loop",
                vec![node_id],
            )),
        }
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
                            let (target_id, location) =
                                match self.closest_variable(node_id, &ident)? {
                                    Some(node_id) => node_id,
                                    None => {
                                        // let ident = ident.into();
                                        // self.add_runtime_variable(ident)?
                                        Err(self.ast.error(
                                            "Failed to find variable to assign to",
                                            "variable not found",
                                            vec![node_id],
                                        ))?
                                    }
                                };
                            self.add_ref(target_id, node_id, ReceiveValue, location);
                            NodeBody::VariableAssignment(target_id, expr_id)
                        }
                        VariableValue(ident) => {
                            let (target_id, location) =
                                match self.closest_variable(node_id, &ident)? {
                                    Some(node_id) => node_id,
                                    None => {
                                        // let ident = ident.into();
                                        // self.add_runtime_variable(ident)?
                                        Err(self.ast.error(
                                            "Failed to find variable",
                                            "variable not found",
                                            vec![node_id],
                                        ))?
                                    }
                                };
                            self.add_ref(target_id, node_id, AssignValue, location);
                            NodeBody::VariableValue(target_id)
                        }
                        Call(ident, _) => {
                            let (target_id, location) =
                                match self.closest_variable(node_id, &ident)? {
                                    Some(node_id) => node_id,
                                    None => Err(self.ast.error(
                                        "Failed to find variable to call",
                                        "function not found",
                                        vec![node_id],
                                    ))?,
                                };
                            // We move the args out the old NodeBody so that we do not have to copy
                            // the argument vec while replacing the node body.
                            let node = self.ast.get_node_mut(node_id);
                            let args = match &mut node.body {
                                NodeBody::Unlinked(Call(_, args)) => mem::replace(args, Vec::new()),
                                _ => unreachable!(),
                            };
                            self.add_ref(target_id, node_id, GoTo, location);
                            NodeBody::Call(target_id, args)
                        }
                        ImportValue(ident) => {
                            let mut body = None;
                            for (i, func) in self.runtime.functions.iter().enumerate() {
                                if &func.name == ident {
                                    body = Some(NodeBody::ConstValue(NodeValue::RuntimeFn(i)));
                                    break;
                                }
                            }
                            if let Some(body) = body {
                                body
                            } else {
                                Err(self.ast.error(
                                    "Failed to find import",
                                    "import value not found",
                                    vec![node_id],
                                ))?
                            }
                        }
                        Return(_) => {
                            let (target_id, location) = self.closest_fn(node_id)?;
                            self.add_ref(target_id, node_id, GoTo, location);
                            let node = self.ast.get_node_mut(node_id);
                            let ret = match &mut node.body {
                                NodeBody::Unlinked(Return(ret)) => mem::replace(ret, None),
                                _ => unreachable!(),
                            };
                            NodeBody::Return(target_id, ret)
                        }
                        Break => {
                            let (target_id, location) = self.closest_loop(node_id)?;
                            self.add_ref(target_id, node_id, GoTo, location);
                            NodeBody::Break(target_id)
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
