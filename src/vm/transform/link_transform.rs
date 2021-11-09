use crate::vm::ast::NodeReferenceType::*;
use crate::vm::ast::UnlinkedNodeBody::*;
use crate::vm::ast::{Ast, Linked, NodeReferenceLocation, PartialNodeValue, PartialType};
use crate::vm::ast::{AstBranch, IsValid, NodeID};
use crate::vm::ast::{Err, ErrPart, NodeReferenceType, NodeType, NodeValue};
use crate::vm::ast::{NBCall, NodeBody};
use crate::vm::runtime::FunctionDefinition;
use crate::vm::{ast, transform};
use crate::{vm, PathKey};
use std::borrow::Borrow;
use std::collections::{HashMap, VecDeque};
use std::fmt::Debug;
use std::mem;
use std::sync::{Arc, RwLock};

#[derive(Debug)]
struct PendingRef {
    target: (NodeID, NodeReferenceType),
    referencer: (NodeID, NodeReferenceType),
    loc: NodeReferenceLocation,
}

pub struct Link<'a> {
    tokio_runtime: &'a tokio::runtime::Runtime,
    vm_runtime: &'a vm::Runtime,
}

impl<'a> Link<'a> {
    pub fn new(tokio_runtime: &'a tokio::runtime::Runtime, vm_runtime: &'a vm::Runtime) -> Self {
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
            let exports = ast
                .paths()
                .map(|(path, ast)| (path.clone(), ast.exports()))
                .collect::<HashMap<_, _>>();
            let exports = Arc::new(exports);
            let (tx, mut rx) = tokio::sync::mpsc::unbounded_channel();

            let asts = Arc::new(RwLock::new(ast));
            for id in asts.read().unwrap().ids() {
                let vm_runtime = self.vm_runtime.definitions.clone();
                let tx = tx.clone();
                let asts = asts.clone();
                let exports = exports.clone();
                tokio::task::spawn_blocking(move || {
                    let asts = asts.read().unwrap();
                    let vm_runtime = vm_runtime.borrow();
                    let mut ast = asts.write_ast(id);
                    let root_id = ast.root();
                    let linker = Linker::new(&mut ast, vm_runtime, &exports);
                    match linker.link(root_id) {
                        Ok(pending) => tx.send(Ok(pending)).unwrap(),
                        Err(e) => tx.send(Err(e)).unwrap(),
                    }
                });
            }

            drop(tx);
            let mut pending_refs = Vec::new();
            let mut errors = Vec::new();
            while let Some(msg) = rx.recv().await {
                match msg {
                    Ok(pending) => pending_refs.push(pending),
                    Err(err) => errors.push(err),
                }
            }

            let mut asts = Arc::try_unwrap(asts)
                .unwrap()
                .into_inner()
                .expect("Single instance of ast in link transform");
            if let Some(err) = errors.pop() {
                Err((asts.guarantee_state(), err))
            } else {
                for outer in pending_refs {
                    for pending in outer {
                        asts.add_ref(pending.target, pending.referencer, pending.loc);
                    }
                }
                Ok(asts.guarantee_state())
            }
        })
    }
}

struct Linker<'a, 'b, T>
where
    T: IsValid,
{
    ast: &'a mut AstBranch<T>,
    runtime: &'b Vec<FunctionDefinition>,
    exports: &'b HashMap<PathKey, HashMap<String, (NodeID, bool)>>,
}

impl<'a, 'b, T> Linker<'a, 'b, T>
where
    T: IsValid,
{
    fn new(
        ast: &'a mut AstBranch<T>,
        runtime: &'b Vec<FunctionDefinition>,
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
                            for (i, func) in self.runtime.iter().enumerate() {
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
                                                pending_refs.push(PendingRef {
                                                    target: (
                                                        *reference_id,
                                                        NodeReferenceType::ReadExternalValue,
                                                    ),
                                                    referencer: (
                                                        node_id,
                                                        NodeReferenceType::WriteExternalValue,
                                                    ),
                                                    loc: self.loc_relative_to_root(node_id),
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
        Ok(pending_refs)
    }
}
