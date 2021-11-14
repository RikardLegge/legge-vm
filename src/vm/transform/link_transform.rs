use crate::vm::ast::NodeReferenceType::*;
use crate::vm::ast::UnlinkedNodeBody::*;
use crate::vm::ast::{
    Ast, Linked, LinkedNodeBody, NodeReferenceLocation, PartialNodeValue, PartialType,
};
use crate::vm::ast::{AstBranch, IsValid, NodeID};
use crate::vm::ast::{Err, ErrPart, NodeReferenceType, NodeType, NodeValue};
use crate::vm::ast::{NBCall, PartialNodeBody};
use crate::vm::runtime::{Namespace, NamespaceElement, RuntimeDefinitions};
use crate::vm::{ast, transform};
use std::collections::VecDeque;
use std::fmt::Debug;
use std::mem;
use std::sync::Arc;

#[derive(Debug)]
struct PendingRef {
    target: (NodeID, NodeReferenceType),
    referencer: (NodeID, NodeReferenceType),
    loc: NodeReferenceLocation,
}

pub struct Link<'a> {
    tokio_runtime: &'a tokio::runtime::Runtime,
    vm_runtime: Arc<RuntimeDefinitions>,
}

impl<'a> Link<'a> {
    pub fn new(
        tokio_runtime: &'a tokio::runtime::Runtime,
        vm_runtime: Arc<RuntimeDefinitions>,
    ) -> Self {
        Self {
            tokio_runtime,
            vm_runtime,
        }
    }
}

impl<'a, T> transform::AstTransformation<T, Linked> for Link<'a>
where
    T: IsValid,
{
    fn name(&self) -> String {
        "Link Nodes".to_string()
    }
    fn transform(&self, ast: Ast<T>) -> transform::Result<Linked> {
        self.tokio_runtime.block_on(async {
            let mut exports = Namespace::new();
            for (path, ast) in ast.paths() {
                exports.set(path.as_ref(), NamespaceElement::Namespace(ast.exports()));
            }

            let exports = Arc::new(exports);
            let asts = Arc::new(ast);

            let tasks = asts.ids().map(|id| {
                let vm_runtime = self.vm_runtime.clone();
                let asts = asts.clone();
                let exports = exports.clone();
                tokio::task::spawn_blocking(move || {
                    let vm_runtime = &vm_runtime;
                    let mut ast = asts.write_ast(id);
                    let root_id = ast.root();
                    let linker = Linker::new(&mut ast, vm_runtime, &exports);
                    match linker.link(root_id) {
                        Ok(pending) => Ok(pending),
                        Err(e) => Err(e),
                    }
                })
            });

            let mut pending_refs = Vec::with_capacity(asts.len());
            let task_results = futures::future::try_join_all(tasks).await.unwrap();
            let mut asts = Arc::try_unwrap(asts).expect("Single instance of ast in link transform");
            for res in task_results {
                match res {
                    Ok(refs) => pending_refs.push(refs),
                    Err(err) => return Err((asts.guarantee_state(), err)),
                }
            }

            for outer in pending_refs {
                for pending in outer {
                    asts.add_ref(pending.target, pending.referencer, pending.loc);
                }
            }
            Ok(asts.guarantee_state())
        })
    }
}

struct Linker<'a, 'b, T>
where
    T: IsValid,
{
    ast: &'a mut AstBranch<T>,
    runtime: &'b RuntimeDefinitions,
    exports: &'b Namespace,
}

impl<'a, 'b, T> Linker<'a, 'b, T>
where
    T: IsValid,
{
    fn new(
        ast: &'a mut AstBranch<T>,
        runtime: &'b RuntimeDefinitions,
        exports: &'b Namespace,
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
    ) -> ast::Result<Option<(NodeID, NodeReferenceLocation)>> {
        self.ast.closest_variable(node_id, ident)
    }

    fn closest_fn(&mut self, node_id: NodeID) -> ast::Result<(NodeID, NodeReferenceLocation)> {
        match self.ast.closest_fn(node_id) {
            Some(id) => Ok(id),
            _ => Err(Err::single(
                "No function ancestor found starting at",
                "statement outside of function",
                vec![node_id],
            )),
        }
    }

    fn closest_loop(&mut self, node_id: NodeID) -> ast::Result<(NodeID, NodeReferenceLocation)> {
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
    ) -> ast::Result<PartialNodeValue<T>> {
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
                    PartialNodeBody::Linked(LinkedNodeBody::TypeDeclaration {
                        default_value,
                        ..
                    }) => match default_value {
                        Some(default_value) => (*default_value).clone(),
                        None => unreachable!(),
                    },
                    _ => unreachable!(),
                }
            }
            _ => unreachable!(),
        })
    }

    fn resolve_type(&mut self, tp: NodeType, parts: &[NodeID]) -> ast::Result<Option<NodeType>> {
        let tp = match tp {
            NodeType::Void
            | NodeType::Int
            | NodeType::Float
            | NodeType::Bool
            | NodeType::Any
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

    fn link_types(&mut self, node_id: NodeID) -> ast::Result<Option<NodeType>> {
        let node = self.ast.get_node(node_id);
        let tp = match &node.body {
            PartialNodeBody::Linked(body) => match body {
                LinkedNodeBody::TypeDeclaration { tp, .. } => {
                    let tp = *tp;
                    self.link_types(tp)?
                }
                LinkedNodeBody::PartialType { tp, parts } => match tp {
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
                                node.body =
                                    PartialNodeBody::Linked(LinkedNodeBody::TypeReference {
                                        tp: tp_id,
                                    });
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
                                node.body = PartialNodeBody::Linked(LinkedNodeBody::PartialType {
                                    tp: PartialType::Complete(tp.clone()),
                                    parts,
                                });
                                Some(tp)
                            }
                            None => None,
                        }
                    }
                },
                _ => None,
            },
            _ => None,
        };
        Ok(tp)
    }

    fn loc_relative_to_root(&self, mut node_id: NodeID) -> NodeReferenceLocation {
        loop {
            let node = self.ast.get_node(node_id);
            if let Some(parent_id) = node.parent_id {
                if node.is_closure_boundary() {
                    break NodeReferenceLocation::Closure;
                }
                node_id = parent_id;
            } else {
                break NodeReferenceLocation::Local;
            }
        }
    }

    fn link(mut self, root_id: NodeID) -> ast::Result<Vec<PendingRef>> {
        let mut pending_refs = Vec::new();
        let mut queue = VecDeque::from(vec![root_id]);
        while let Some(node_id) = queue.pop_front() {
            let node = self.ast.get_node(node_id);
            for &child in node.body.children() {
                queue.push_back(child);
            }
            let unlinked_body = if let PartialNodeBody::Unlinked(_) = node.body {
                let node = self.ast.get_node_mut(node_id);
                let body = mem::replace(&mut node.body, PartialNodeBody::Empty);
                match body {
                    PartialNodeBody::Unlinked(unlinked_body) => Some(unlinked_body),
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
                        LinkedNodeBody::VariableAssignment {
                            variable,
                            path,
                            expr,
                        }
                    }
                    Value { value, tp } => {
                        let value = self.resolve_value(node_id, value)?;
                        LinkedNodeBody::ConstValue { tp, value }
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
                        LinkedNodeBody::VariableValue { variable, path }
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
                        LinkedNodeBody::Call(NBCall { func, args })
                    }
                    ImportValue { is_relative, path } => {
                        if is_relative {
                            let parent = match self.ast.path.not_last() {
                                Some(parent_path) => match self.exports.get(parent_path) {
                                    Some(NamespaceElement::Namespace(n)) => n,
                                    _ => unimplemented!(),
                                },
                                None => self.exports,
                            };
                            match parent.get(path.as_ref()) {
                                Some(NamespaceElement::Namespace(_)) => unimplemented!(),
                                Some(NamespaceElement::BuiltIn(_)) => unimplemented!(),
                                Some(NamespaceElement::Export(export)) => {
                                    if export.is_static {
                                        pending_refs.push(PendingRef {
                                            target: (
                                                export.node_id,
                                                NodeReferenceType::ReadExternalValue,
                                            ),
                                            referencer: (
                                                node_id,
                                                NodeReferenceType::WriteExternalValue,
                                            ),
                                            loc: self.loc_relative_to_root(node_id),
                                        });
                                        LinkedNodeBody::Reference {
                                            node_id: export.node_id,
                                        }
                                    } else {
                                        Err(Err::new(
                                            "Invalid import: Only allowed to import static declarations like types, constants or functions".to_string(),
                                            vec![
                                                ErrPart::new("Imported here".to_string(), vec![node_id]),
                                                ErrPart::new("This value is not static".to_string(), vec![export.node_id]),
                                            ]
                                        ))?
                                    }
                                }
                                None => unreachable!("Invalid path {:?}", path),
                            }
                        } else {
                            match self.runtime.namespace.get(path.as_ref()) {
                                Some(NamespaceElement::Namespace(_)) => unimplemented!(),
                                Some(NamespaceElement::Export(_)) => unimplemented!(),
                                Some(NamespaceElement::BuiltIn(built_in)) => built_in.body(),
                                None => Err(Err::single(
                                    "Import not available in the built in runtime",
                                    "Imported here",
                                    vec![node_id],
                                ))?,
                            }
                        }
                    }
                    Return { expr, automatic } => {
                        let (func, location) = self.closest_fn(node_id)?;
                        self.ast
                            .add_ref((func, GoTo), (node_id, ControlFlow), location);
                        LinkedNodeBody::Return {
                            func,
                            expr,
                            automatic,
                        }
                    }
                    Break => {
                        let (r#loop, location) = self.closest_loop(node_id)?;
                        self.ast
                            .add_ref((r#loop, GoTo), (node_id, ControlFlow), location);
                        LinkedNodeBody::Break { r#loop }
                    }
                };
                let node = self.ast.get_node_mut(node_id);
                node.body = PartialNodeBody::Linked(linked_body);
            }

            let node = self.ast.get_node(node_id);
            if let None = node.maybe_tp() {
                self.link_types(node_id)?;
            }
        }
        Ok(pending_refs)
    }
}
