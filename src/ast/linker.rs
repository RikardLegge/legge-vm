use super::NodeReferenceType::*;
use super::{Ast, NodeID, Result};
use crate::ast::ast::{NodeReferenceLocation, PartialType, StateLinked};
use crate::ast::nodebody::UnlinkedNodeBody::*;
use crate::ast::nodebody::{NBCall, NodeBody};
use crate::ast::{Any, Err, NodeType, NodeValue};
use crate::runtime::Runtime;
use std::collections::VecDeque;
use std::fmt::Debug;
use std::{mem, result};

pub fn link<T>(
    mut ast: Ast<T>,
    runtime: &Runtime,
) -> result::Result<Ast<StateLinked>, (Ast<T>, Err)>
where
    T: Any + Debug,
{
    let root_id = ast.root();
    let linker = Linker::new(&mut ast, runtime);
    match linker.link(root_id) {
        Ok(()) => Ok(unsafe { mem::transmute::<Ast<T>, Ast<StateLinked>>(ast) }),
        Err(err) => Err((ast, err)),
    }
}

struct Linker<'a, 'b, T>
where
    T: Any + Debug,
{
    ast: &'a mut Ast<T>,
    runtime: &'b Runtime,
}

impl<'a, 'b, T> Linker<'a, 'b, T>
where
    T: Any + Debug,
{
    fn new(ast: &'a mut Ast<T>, runtime: &'b Runtime) -> Self {
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

    fn resolve_value(&mut self, node_id: NodeID, value: NodeValue) -> Result<NodeValue> {
        match value {
            NodeValue::Int(_)
            | NodeValue::Bool(_)
            | NodeValue::String(_)
            | NodeValue::Float(_)
            | NodeValue::RuntimeFn(_) => Ok(value),
            NodeValue::Struct(fields) => {
                let mut new_fields = Vec::with_capacity(fields.len());
                for (name, field) in fields {
                    let new_value = self.resolve_value(node_id, field)?;
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

    fn resolve_type(&mut self, tp: NodeType, parts: &[NodeID]) -> Result<Option<NodeType>> {
        let tp = match tp {
            NodeType::Void
            | NodeType::Int
            | NodeType::Float
            | NodeType::Bool
            | NodeType::Any
            | NodeType::NotYetImplemented
            | NodeType::String => Some(tp),

            NodeType::VarArg { mut args } => match self.resolve_type(*args, parts)? {
                Some(new_args) => {
                    *args = new_args;
                    Some(NodeType::VarArg { args })
                }
                None => None,
            },

            NodeType::Fn {
                mut args,
                mut returns,
            } => {
                for i in 0..args.len() {
                    match self.fix_unknown_types(parts[i])? {
                        Some(tp) => args[i] = tp,
                        None => return Ok(None),
                    }
                }

                match self.fix_unknown_types(*parts.last().unwrap())? {
                    Some(tp) => *returns = tp,
                    None => return Ok(None),
                }
                Some(NodeType::Fn { args, returns })
            }
            NodeType::Type {
                content: mut tp,
                ident,
            } => match self.resolve_type(*tp, parts)? {
                Some(new_tp) => {
                    *tp = new_tp;
                    Some(NodeType::Type { content: tp, ident })
                }
                None => None,
            },
            NodeType::NewType { mut tp } => match self.resolve_type(*tp, parts)? {
                Some(new_tp) => {
                    *tp = new_tp;
                    Some(NodeType::NewType { tp })
                }
                None => None,
            },
            NodeType::Struct { mut fields } => {
                for i in 0..fields.len() {
                    match self.fix_unknown_types(parts[i])? {
                        Some(tp) => fields[i].1 = tp,
                        None => return Ok(None),
                    };
                }
                Some(NodeType::Struct { fields })
            }
            NodeType::Unknown { .. } => unreachable!(),
        };
        Ok(tp)
    }

    fn fix_unknown_types(&mut self, node_id: NodeID) -> Result<Option<NodeType>> {
        let node = self.ast.get_node(node_id);
        let tp = match &node.body {
            NodeBody::TypeDeclaration { tp, .. } => {
                let tp = *tp;
                self.fix_unknown_types(tp)?
            }
            NodeBody::PartialType { tp, parts } => match tp {
                PartialType::Complete(tp) => Some(tp.clone()),
                PartialType::Uncomplete(NodeType::Unknown { ident }) => {
                    let (target_id, _) = match self.closest_variable(node_id, &ident)? {
                        Some(target) => target,
                        None => Err(self.ast.error(
                            "Failed to find type",
                            "unknown type",
                            vec![node_id],
                        ))?,
                    };
                    match self.ast.partial_type(target_id) {
                        Some((tp_id, tp)) => {
                            let tp = tp.clone();
                            let node = self.ast.get_node_mut(node_id);
                            node.body = NodeBody::TypeReference { tp: tp_id };
                            Some(tp)
                        }
                        None => None,
                    }
                }
                PartialType::Uncomplete(tp) => {
                    let tp = tp.clone();
                    let parts = parts.clone();
                    match self.resolve_type(tp, &parts)? {
                        Some(tp) => {
                            let node = self.ast.get_node_mut(node_id);
                            node.body = NodeBody::PartialType {
                                tp: PartialType::Complete(tp.clone()),
                                parts,
                            };
                            Some(tp)
                        }
                        None => None,
                    }
                }
            },
            _ => None,
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
            if let NodeBody::Unlinked(body) = &node.body {
                let linked_body = match body {
                    VariableAssignment { ident, path, expr } => {
                        let expr = *expr;
                        let (variable, location) = match self.closest_variable(node_id, &ident)? {
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
                    Value { value, tp } => {
                        let tp = tp.clone();
                        let value = value.clone();
                        let new_value = self.resolve_value(node_id, value)?;
                        NodeBody::ConstValue {
                            tp,
                            value: new_value,
                        }
                    }
                    VariableValue { ident, path } => {
                        let (variable, location) = match self.closest_variable(node_id, &ident)? {
                            Some(target) => target,
                            None => Err(self.ast.error(
                                "Failed to find variable",
                                "variable not found",
                                vec![node_id],
                            ))?,
                        };
                        let path = path.clone();
                        self.ast
                            .add_ref((variable, ReadValue), (node_id, WriteValue), location);
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
                            NodeBody::Unlinked(Call { args, .. }) => mem::replace(args, Vec::new()),
                            _ => unreachable!(),
                        };
                        self.ast
                            .add_ref((func, GoTo), (node_id, ExecuteValue), location);
                        NodeBody::Call(NBCall { func, args })
                    }
                    ImportValue { ident } => {
                        let mut body = None;
                        for (i, func) in self.runtime.functions.iter().enumerate() {
                            if &func.name == ident {
                                body = Some(NodeBody::ConstValue {
                                    tp: None,
                                    value: NodeValue::RuntimeFn(i),
                                });
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
                        let (expr, automatic) = match &mut node.body {
                            NodeBody::Unlinked(Return { expr, automatic }) => {
                                (mem::replace(expr, None), *automatic)
                            }
                            _ => unreachable!(),
                        };
                        NodeBody::Return {
                            func,
                            expr,
                            automatic,
                        }
                    }
                    Break => {
                        let (r#loop, location) = self.closest_loop(node_id)?;
                        self.ast
                            .add_ref((r#loop, GoTo), (node_id, ControlFlow), location);
                        NodeBody::Break { r#loop }
                    }
                };
                let node = self.ast.get_node_mut(node_id);
                node.body = linked_body;
            }

            let node = self.ast.get_node(node_id);
            if let None = node.tp {
                self.fix_unknown_types(node_id)?;
            }
        }
        Ok(())
    }
}
