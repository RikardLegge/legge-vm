use super::NodeReferenceType::*;
use super::UnlinkedNodeBody::*;
use super::{Ast, NodeBody, NodeID, Result};
use crate::ast::ast::NodeReferenceLocation;
use crate::ast::{NodeType, NodeValue};
use crate::runtime::Runtime;
use std::collections::VecDeque;
use std::mem;

pub fn link(ast: &mut Ast, runtime: &Runtime) -> Result<()> {
    let root_id = ast.root();
    Linker::new(ast, runtime).link(root_id)
}

struct Linker<'a, 'b> {
    ast: &'a mut Ast,
    runtime: &'b Runtime,
}

impl<'a, 'b> Linker<'a, 'b> {
    fn new(ast: &'a mut Ast, runtime: &'b Runtime) -> Self {
        Self { ast, runtime }
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

    fn fix_unknown_fields(&mut self, node_id: NodeID, value: NodeValue) -> Result<NodeValue> {
        match value {
            NodeValue::Int(_)
            | NodeValue::Bool(_)
            | NodeValue::String(_)
            | NodeValue::Float(_)
            | NodeValue::RuntimeFn(_) => Ok(value),
            NodeValue::Struct(fields) => {
                let mut new_fields = Vec::with_capacity(fields.len());
                for (name, field) in fields {
                    let new_value = self.fix_unknown_fields(node_id, field)?;
                    new_fields.push((name, new_value));
                }
                Ok(NodeValue::Struct(new_fields))
            }
            NodeValue::Unlinked(ident) => {
                let (target_id, _) = match self.closest_variable(node_id, &ident)? {
                    Some(target) => target,
                    None => Err(self.ast.error(
                        "Failed to find type",
                        "value not found",
                        vec![node_id],
                    ))?,
                };
                let value = match &self.ast.get_node(target_id).body {
                    NodeBody::TypeDeclaration { default_value, .. } => match default_value {
                        Some(default_value) => default_value.clone(),
                        None => unreachable!(),
                    },
                    _ => unreachable!(),
                };
                Ok(value)
            }
        }
    }

    fn fix_unknown_types(&self, node_id: NodeID, tp: NodeType) -> Result<NodeType> {
        let tp = match tp {
            NodeType::Void | NodeType::Int | NodeType::Bool | NodeType::String => tp,
            NodeType::Fn { .. } => unimplemented!(),
            NodeType::Type { tp } => NodeType::Type {
                tp: Box::new(self.fix_unknown_types(node_id, *tp)?),
            },
            NodeType::Struct { fields } => {
                let mut fixed_fields = Vec::with_capacity(fields.len());
                for (name, tp) in fields {
                    let new_value = self.fix_unknown_types(node_id, tp)?;
                    fixed_fields.push((name, new_value));
                }
                NodeType::Struct {
                    fields: fixed_fields,
                }
            }
            NodeType::Unknown { ident } => {
                let (target_id, _) = match self.closest_variable(node_id, &ident)? {
                    Some(target) => target,
                    None => {
                        Err(self
                            .ast
                            .error("Failed to find type", "unknown type", vec![node_id]))?
                    }
                };
                match &self.ast.get_node(target_id).body {
                    NodeBody::TypeDeclaration {
                        tp: NodeType::Type { tp },
                        ..
                    } => (**tp).clone(),
                    _ => unreachable!(),
                }
            }
            _ => unimplemented!("{:?}", tp),
        };
        Ok(tp)
    }

    fn link(mut self, root_id: NodeID) -> Result<()> {
        let mut queue = VecDeque::from(vec![root_id]);
        while let Some(node_id) = queue.pop_front() {
            let node = self.ast.get_node(node_id);
            for &child in node.body.children() {
                queue.push_back(child);
            }
            let new_body = match &node.body {
                NodeBody::Unlinked(body) => {
                    let new_body = match body {
                        VariableAssignment { ident, path, expr } => {
                            let expr = *expr;
                            let (variable, location) =
                                match self.closest_variable(node_id, &ident)? {
                                    Some(node_id) => node_id,
                                    None => Err(self.ast.error(
                                        "Failed to find variable to assign to",
                                        "variable not found",
                                        vec![node_id],
                                    ))?,
                                };
                            let path = path.clone();
                            self.ast
                                .add_ref((variable, WriteValue), (expr, ReadValue), location);
                            NodeBody::VariableAssignment {
                                variable,
                                path,
                                expr,
                            }
                        }
                        Value(value) => {
                            let value = value.clone();
                            let new_value = self.fix_unknown_fields(node_id, value)?;
                            NodeBody::ConstValue(new_value)
                        }
                        Type { def, .. } => match &**def {
                            NodeBody::TypeDeclaration {
                                ident,
                                tp,
                                constructor,
                                default_value,
                            } => {
                                let tp = self.fix_unknown_types(node_id, tp.clone())?;
                                NodeBody::TypeDeclaration {
                                    ident: ident.clone(),
                                    tp,
                                    constructor: *constructor,
                                    default_value: default_value.clone(),
                                }
                            }
                            NodeBody::ProcedureDeclaration {
                                args,
                                returns,
                                body,
                            } => {
                                let tp =
                                    self.fix_unknown_types(node_id, returns.clone().unwrap())?;
                                NodeBody::ProcedureDeclaration {
                                    args: args.clone(),
                                    returns: Some(tp),
                                    body: body.clone(),
                                }
                            }
                            _ => unimplemented!("{:?}", def),
                        },
                        VariableValue { ident, path } => {
                            let (variable, location) =
                                match self.closest_variable(node_id, &ident)? {
                                    Some(target) => target,
                                    None => Err(self.ast.error(
                                        "Failed to find variable",
                                        "variable not found",
                                        vec![node_id],
                                    ))?,
                                };
                            let path = path.clone();
                            self.ast.add_ref(
                                (variable, ReadValue),
                                (node_id, WriteValue),
                                location,
                            );
                            NodeBody::VariableValue { variable, path }
                        }
                        Call { ident, .. } => {
                            let (func, location) = match self.closest_variable(node_id, &ident)? {
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
                                NodeBody::Unlinked(Call { args, .. }) => {
                                    mem::replace(args, Vec::new())
                                }
                                _ => unreachable!(),
                            };
                            self.ast
                                .add_ref((func, GoTo), (node_id, ExecuteValue), location);
                            NodeBody::Call { func, args }
                        }
                        ImportValue { ident } => {
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
                        Return { .. } => {
                            let (func, location) = self.closest_fn(node_id)?;
                            self.ast
                                .add_ref((func, GoTo), (node_id, ControlFlow), location);
                            let node = self.ast.get_node_mut(node_id);
                            let expr = match &mut node.body {
                                NodeBody::Unlinked(Return { expr }) => mem::replace(expr, None),
                                _ => unreachable!(),
                            };
                            NodeBody::Return {
                                func,
                                expr,
                                automatic: false,
                            }
                        }
                        Break => {
                            let (r#loop, location) = self.closest_loop(node_id)?;
                            self.ast
                                .add_ref((r#loop, GoTo), (node_id, ControlFlow), location);
                            NodeBody::Break { r#loop }
                        }
                    };
                    Some(new_body)
                }

                NodeBody::Empty
                | NodeBody::Loop { .. }
                | NodeBody::Op { .. }
                | NodeBody::PrefixOp { .. }
                | NodeBody::Block { .. }
                | NodeBody::If { .. }
                | NodeBody::Expression { .. }
                | NodeBody::Comment { .. }
                | NodeBody::Import { .. }
                | NodeBody::ConstValue { .. }
                | NodeBody::VariableAssignment { .. }
                | NodeBody::VariableValue { .. }
                | NodeBody::Break { .. }
                | NodeBody::Call { .. }
                | NodeBody::TypeDeclaration { .. } => None,

                NodeBody::Return { func, .. } => {
                    let func = *func;
                    self.ast.add_ref(
                        (func, GoTo),
                        (node_id, ControlFlow),
                        NodeReferenceLocation::Local,
                    );
                    None
                }

                NodeBody::ProcedureDeclaration {
                    args,
                    returns,
                    body,
                } => {
                    if let Some(returns) = returns {
                        let tp = self.fix_unknown_types(node_id, returns.clone())?;
                        Some(NodeBody::ProcedureDeclaration {
                            args: args.clone(),
                            returns: Some(tp),
                            body: *body,
                        })
                    } else {
                        None
                    }
                }
                NodeBody::VariableDeclaration { ident, tp, expr } => {
                    if let Some(NodeType::Unknown { .. }) = *tp {
                        let tp = self.fix_unknown_types(node_id, tp.clone().unwrap())?;
                        Some(NodeBody::VariableDeclaration {
                            ident: ident.clone(),
                            tp: Some(tp),
                            expr: *expr,
                        })
                    } else {
                        None
                    }
                }
                NodeBody::ConstDeclaration { ident, tp, expr } => {
                    if let Some(NodeType::Unknown { .. }) = *tp {
                        let tp = self.fix_unknown_types(node_id, tp.clone().unwrap())?;
                        Some(NodeBody::ConstDeclaration {
                            ident: ident.clone(),
                            tp: Some(tp),
                            expr: *expr,
                        })
                    } else {
                        None
                    }
                }
                NodeBody::StaticDeclaration { ident, tp, expr } => {
                    if let Some(NodeType::Unknown { .. }) = *tp {
                        let tp = self.fix_unknown_types(node_id, tp.clone().unwrap())?;
                        Some(NodeBody::StaticDeclaration {
                            ident: ident.clone(),
                            tp: Some(tp),
                            expr: *expr,
                        })
                    } else {
                        None
                    }
                }
            };
            if let Some(body) = new_body {
                let node = self.ast.get_node_mut(node_id);
                node.body = body;
            }
        }
        Ok(())
    }
}
