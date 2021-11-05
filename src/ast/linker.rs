use super::NodeReferenceType::*;
use super::{Ast, NodeID};
use crate::ast::ast::{
    AstCollection, NodeReferenceLocation, PartialNodeValue, PartialType, StateLinked,
};
use crate::ast::nodebody::UnlinkedNodeBody::*;
use crate::ast::nodebody::{NBCall, NodeBody};
use crate::ast::{Err, ErrPart, NodeType, NodeValue, PathKey, Result};
use crate::runtime::Runtime;
use std::collections::{HashMap, VecDeque};
use std::fmt::Debug;
use std::{mem, result};

pub fn link<T>(
    mut asts: AstCollection<T>,
    runtime: &Runtime,
) -> result::Result<AstCollection<StateLinked>, (AstCollection<T>, Err)>
where
    T: Debug,
{
    let exports = asts
        .named()
        .map(|(path, ast)| (path.clone(), ast.borrow().exports()))
        .collect::<HashMap<_, _>>();
    let mut err = None;
    for ast in asts.iter_mut() {
        let root_id = ast.borrow().root();
        let mut ast = ast.borrow_mut();
        let linker = Linker::new(&mut ast, runtime, &exports);
        if let Err(e) = linker.link(root_id) {
            err = Some(e);
            break;
        }
    }
    match err {
        Some(err) => Err((asts, err)),
        None => Ok(asts.guarantee_state()),
    }
}

struct Linker<'a, 'b, T>
where
    T: Debug,
{
    ast: &'a mut Ast<T>,
    runtime: &'b Runtime,
    exports: &'b HashMap<PathKey, HashMap<String, (NodeID, bool)>>,
}

impl<'a, 'b, T> Linker<'a, 'b, T>
where
    T: Debug,
{
    fn new(
        ast: &'a mut Ast<T>,
        runtime: &'b Runtime,
        exports: &'b HashMap<PathKey, HashMap<String, (NodeID, bool)>>,
    ) -> Self {
        Self {
            ast,
            runtime,
            exports,
        }
    }

    fn closest_variable(
        &self,
        node_id: NodeID,
        ident: &str,
    ) -> Result<Option<(NodeID, NodeReferenceLocation)>> {
        match self.ast.closest_variable(node_id, ident) {
            Ok(v) => Ok(v),
            Err(err) => Err(Err::new(err.details, err.parts)),
        }
    }

    fn closest_fn(&mut self, node_id: NodeID) -> Result<(NodeID, NodeReferenceLocation)> {
        match self.ast.closest_fn(node_id) {
            Some(id) => Ok(id),
            _ => Err(Err::single(
                "No function ancestor found starting at",
                "statement outside of function",
                vec![node_id],
            )),
        }
    }

    fn closest_loop(&mut self, node_id: NodeID) -> Result<(NodeID, NodeReferenceLocation)> {
        match self.ast.closest_loop(node_id) {
            Some(id) => Ok(id),
            _ => Err(Err::single(
                "No loop ancestor found starting at",
                "statement outside of loop",
                vec![node_id],
            )),
        }
    }

    fn resolve_value(
        &mut self,
        node_id: NodeID,
        value: PartialNodeValue<T>,
    ) -> Result<PartialNodeValue<T>> {
        Ok(match value {
            PartialNodeValue::Linked(value) => match value {
                NodeValue::Int(_)
                | NodeValue::Bool(_)
                | NodeValue::String(_)
                | NodeValue::Float(_)
                | NodeValue::RuntimeFn(_) => value.into(),
                NodeValue::Struct(fields) => {
                    let mut new_fields = Vec::with_capacity(fields.len());
                    for (name, field) in fields {
                        let new_value = self.resolve_value(node_id, field)?;
                        new_fields.push((name, new_value));
                    }
                    NodeValue::Struct(new_fields).into()
                }
            },
            PartialNodeValue::Unlinked(ident) => {
                let (target_id, _) = match self.closest_variable(node_id, &ident)? {
                    Some(target) => target,
                    None => Err(Err::single(
                        "Failed to find type",
                        "value not found",
                        vec![node_id],
                    ))?,
                };
                match &self.ast.get_node(target_id).body {
                    NodeBody::TypeDeclaration { default_value, .. } => match default_value {
                        Some(default_value) => (*default_value).clone(),
                        None => unreachable!(),
                    },
                    _ => unreachable!(),
                }
            }
            _ => unreachable!(),
        })
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
                    match self.link_types(parts[i])? {
                        Some(tp) => args[i] = tp,
                        None => return Ok(None),
                    }
                }

                match self.link_types(*parts.last().unwrap())? {
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
                    match self.link_types(parts[i])? {
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

    fn link_types(&mut self, node_id: NodeID) -> Result<Option<NodeType>> {
        let node = self.ast.get_node(node_id);
        let tp = match &node.body {
            NodeBody::TypeDeclaration { tp, .. } => {
                let tp = *tp;
                self.link_types(tp)?
            }
            NodeBody::PartialType { tp, parts } => match tp {
                PartialType::Complete(tp) => Some(tp.clone()),
                PartialType::Uncomplete(NodeType::Unknown { ident }) => {
                    let (target_id, _) = match self.closest_variable(node_id, &ident)? {
                        Some(target) => target,
                        None => Err(Err::single(
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
            let unlinked_body = if let NodeBody::Unlinked(_) = node.body {
                let node = self.ast.get_node_mut(node_id);
                let body = mem::replace(&mut node.body, NodeBody::Empty);
                match body {
                    NodeBody::Unlinked(unlinked_body) => Some(unlinked_body),
                    _ => unreachable!(),
                }
            } else {
                None
            };
            if let Some(body) = unlinked_body {
                let linked_body = match body {
                    VariableAssignment { ident, path, expr } => {
                        let (variable, location) = match self.closest_variable(node_id, &ident)? {
                            Some(node_id) => node_id,
                            None => Err(Err::single(
                                "Failed to find variable to assign to",
                                "variable not found",
                                vec![node_id],
                            ))?,
                        };
                        self.ast
                            .add_ref((variable, WriteValue), (expr, ReadValue), location);
                        NodeBody::VariableAssignment {
                            variable,
                            path,
                            expr,
                        }
                    }
                    Value { value, tp } => {
                        let value = self.resolve_value(node_id, value)?;
                        NodeBody::ConstValue { tp, value }
                    }
                    VariableValue { ident, path } => {
                        let (variable, location) = match self.closest_variable(node_id, &ident)? {
                            Some(target) => target,
                            None => Err(Err::single(
                                "Failed to find variable",
                                "variable not found",
                                vec![node_id],
                            ))?,
                        };
                        self.ast
                            .add_ref((variable, ReadValue), (node_id, WriteValue), location);
                        NodeBody::VariableValue { variable, path }
                    }
                    Call { ident, args } => {
                        let (func, location) = match self.closest_variable(node_id, &ident)? {
                            Some(node_id) => node_id,
                            None => Err(Err::single(
                                "Failed to find variable to call",
                                "function not found",
                                vec![node_id],
                            ))?,
                        };
                        self.ast
                            .add_ref((func, GoTo), (node_id, ExecuteValue), location);
                        NodeBody::Call(NBCall { func, args })
                    }
                    ImportValue { path, module } => {
                        let mut body = None;
                        if let [ident] = &path[..] {
                            for (i, func) in self.runtime.functions.iter().enumerate() {
                                if func.name == *ident && func.module == module {
                                    body = Some(NodeBody::ConstValue {
                                        tp: None,
                                        value: NodeValue::RuntimeFn(i).into(),
                                    });
                                    break;
                                }
                            }
                        }
                        if let None = body {
                            if &module == "local" {
                                if let Some((ident, path)) = path.split_last() {
                                    if let Some(export) = self.exports.get(path) {
                                        if let Some((reference_id, valid)) =
                                            export.get(ident.as_str())
                                        {
                                            if *valid {
                                                body = Some(NodeBody::Reference {
                                                    node_id: *reference_id,
                                                });
                                            } else {
                                                Err(Err::new(
                                                    "Invalid import: Only allowed to import static declarations like types, constants or functions".to_string(),
                                                    vec![
                                                        ErrPart::new("Imported here".to_string(), vec![node_id]),
                                                        ErrPart::new("This value is not static".to_string(), vec![*reference_id]),
                                                    ]
                                                ))?
                                            }
                                        }
                                    }
                                }
                            }
                        }
                        if let Some(body) = body {
                            body
                        } else {
                            Err(Err::single(
                                "Failed to find import",
                                "import value not found",
                                vec![node_id],
                            ))?
                        }
                    }
                    Return { expr, automatic } => {
                        let (func, location) = self.closest_fn(node_id)?;
                        self.ast
                            .add_ref((func, GoTo), (node_id, ControlFlow), location);
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
            if let None = node.maybe_tp() {
                self.link_types(node_id)?;
            }
        }
        Ok(())
    }
}
