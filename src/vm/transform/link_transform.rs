use crate::vm::ast::NodeReferenceType::*;
use crate::vm::ast::UnlinkedNodeBody::*;
use crate::vm::ast::{
    Ast, AstBranchID, LNBTypeDeclaration, Linked, LinkedNodeBody, NodeReferenceLocation,
    PartialNodeValue, PartialType, UnlinkedNodeBody,
};
use crate::vm::ast::{AstBranch, IsValid, NodeID};
use crate::vm::ast::{Err, ErrPart, NodeReferenceType, NodeType, NodeValue};
use crate::vm::ast::{NBCall, PartialNodeBody};
use crate::vm::runtime::{Namespace, NamespaceElement, RuntimeDefinitions};
use crate::vm::{ast, transform};
use std::collections::{HashMap, VecDeque};
use std::sync::{Arc, RwLockReadGuard, RwLockWriteGuard};
use std::{mem, result};

#[derive(Debug)]
pub struct PendingRef {
    pub target: (NodeID, NodeReferenceType),
    pub referencer: (NodeID, NodeReferenceType),
    pub loc: NodeReferenceLocation,
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
    fn transform(&self, _: Ast<T>) -> transform::Result<Linked> {
        unimplemented!();
        // self.tokio_runtime.block_on(async {
        //     let mut exports = Namespace::new();
        //     for (path, ast) in ast.paths() {
        //         exports.set(path.as_ref(), NamespaceElement::Namespace(ast.exports()));
        //     }
        //
        //     let exports = Arc::new(exports);
        //     let asts = Arc::new(ast);
        //
        //     let tasks = asts.ids().map(|id| {
        //         let vm_runtime = self.vm_runtime.clone();
        //         let asts = asts.clone();
        //         let exports = exports.clone();
        //         tokio::task::spawn_blocking(move || {
        //             let mut linker = Linker::new(id, asts, vm_runtime, exports);
        //             linker.link()
        //         })
        //     });
        //
        //     let task_results = futures::future::try_join_all(tasks).await.unwrap();
        //     let asts = Arc::try_unwrap(asts).expect("Single instance of ast in link transform");
        //     for res in task_results {
        //         match res {
        //             Ok(_) => {}
        //             Err(err) => return Err((asts.guarantee_state(), err)),
        //         }
        //     }
        //     Ok(asts.guarantee_state())
        // })
    }
}

pub enum LinkerErr {
    BlockedByNodeType(NodeID),
    Fail(ast::Err),
}

pub enum LinkerResult {
    Complete(Vec<PendingRef>),
    BlockedByType {
        blocked: HashMap<NodeID, Vec<NodeID>>,
    },
    BlockedByLink {
        blocked_by: NodeID,
    },
    FailedToMakeProgress {
        blocked: HashMap<NodeID, Vec<NodeID>>,
    },
    Failed(ast::Err),
}

impl From<ast::Err> for LinkerErr {
    fn from(e: Err) -> Self {
        LinkerErr::Fail(e)
    }
}

pub struct Linker<T>
where
    T: IsValid,
{
    pending_refs: Vec<PendingRef>,
    queue: VecDeque<NodeID>,
    ast_id: AstBranchID,
    asts: Arc<Ast<T>>,
    runtime: Arc<RuntimeDefinitions>,
    exports: Arc<Namespace>,
}

impl<T> Linker<T>
where
    T: IsValid,
{
    pub fn new(
        ast_id: AstBranchID,
        asts: Arc<Ast<T>>,
        runtime: Arc<RuntimeDefinitions>,
        exports: Arc<Namespace>,
    ) -> Self {
        let root = asts.read_ast(ast_id).root();
        let pending_refs = Vec::new();
        let queue = VecDeque::from(vec![root]);
        Self {
            queue,
            ast_id,
            pending_refs,
            asts,
            runtime,
            exports,
        }
    }

    fn ast(&self) -> RwLockReadGuard<AstBranch<T>> {
        self.asts.read_ast(self.ast_id)
    }

    fn ast_mut(&mut self) -> RwLockWriteGuard<AstBranch<T>> {
        unsafe {
            // Safety: takes a mutable reference to self and therefore guarantees that there are no other live mutable references
            self.asts.write_ast(self.ast_id)
        }
    }

    fn closest_variable(
        &self,
        node_id: NodeID,
        ident: &str,
    ) -> ast::Result<Option<(NodeID, NodeReferenceLocation, usize)>> {
        self.ast().closest_variable(node_id, ident)
    }

    fn closest_fn(&self, node_id: NodeID) -> ast::Result<(NodeID, NodeReferenceLocation, usize)> {
        match self.ast().closest_fn(node_id) {
            Some(id) => Ok(id),
            _ => Err(Err::single(
                "No function ancestor found starting at",
                "statement outside of function",
                vec![node_id],
            )),
        }
    }

    fn closest_loop(&self, node_id: NodeID) -> ast::Result<(NodeID, NodeReferenceLocation, usize)> {
        match self.ast().closest_loop(node_id) {
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
                let (target_id, _, _) = match self.closest_variable(node_id, &ident)? {
                    Some(target) => target,
                    None => Err(Err::single(
                        "Failed to find type",
                        "value not found",
                        vec![node_id],
                    ))?,
                };
                match &self.ast().get_node(target_id).body {
                    PartialNodeBody::Linked(LinkedNodeBody::TypeDeclaration(
                        LNBTypeDeclaration { default_value, .. },
                    )) => match default_value {
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
                ..
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
        let ast = self.ast();
        let node = ast.get_node(node_id);
        let tp = match &node.body {
            PartialNodeBody::Linked(body) => match body {
                LinkedNodeBody::TypeDeclaration(LNBTypeDeclaration { tp, .. }) => {
                    let tp = *tp;
                    drop(ast);

                    self.link_types(tp)?
                }
                LinkedNodeBody::PartialType { tp, parts } => match tp {
                    PartialType::Complete(tp) => Some(tp.clone()),
                    PartialType::Uncomplete(NodeType::Unknown { ident }) => {
                        let (target_id, _, _) = match self.closest_variable(node_id, &ident)? {
                            Some(target) => target,
                            None => Err(Err::single(
                                "Failed to find type",
                                "unknown type",
                                vec![node_id],
                            ))?,
                        };
                        match ast.partial_type(target_id) {
                            Some((tp_id, tp)) => {
                                let tp = tp.clone();
                                drop(ast);

                                let mut ast = self.ast_mut();
                                let node = ast.get_node_mut(node_id);
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
                        drop(ast);

                        match self.resolve_type(tp, &parts)? {
                            Some(tp) => {
                                let mut ast = self.ast_mut();
                                let node = ast.get_node_mut(node_id);
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
        let ast = self.ast();
        loop {
            let node = ast.get_node(node_id);
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

    pub fn link_all_nodes(&mut self) -> LinkerResult {
        let mut made_progress = false;
        let mut blockers = HashMap::new();

        while let Some(node_id) = self.queue.pop_front() {
            {
                let ast = self.asts.read_ast(self.ast_id);
                let node = ast.get_node(node_id);
                for &child in node.body.children() {
                    self.queue.push_back(child);
                }
                if let PartialNodeBody::Unlinked(_) = node.body {
                } else {
                    continue;
                }
            }
            let partial_body = {
                let mut ast = self.ast_mut();
                let node = ast.get_node_mut(node_id);
                mem::replace(&mut node.body, PartialNodeBody::Empty)
            };
            let unlinked_body = match partial_body {
                PartialNodeBody::Unlinked(unlinked_body) => unlinked_body,
                _ => unreachable!(),
            };
            let linked_body = self.link(node_id, unlinked_body);
            match linked_body {
                Ok(linked_body) => {
                    made_progress = true;
                    let mut ast = self.ast_mut();
                    let node = ast.get_node_mut(node_id);
                    node.body = PartialNodeBody::Linked(linked_body);
                }
                Err(err) => match err {
                    LinkerErr::BlockedByNodeType(blocking_id) => {
                        if !blockers.contains_key(&blocking_id) {
                            blockers.insert(blocking_id, vec![]);
                        }
                        blockers.get_mut(&blocking_id).unwrap().push(node_id);
                    }
                    LinkerErr::Fail(e) => return LinkerResult::Failed(e),
                },
            }
            let missing_tp = {
                let ast = self.ast();
                let node = ast.get_node(node_id);
                node.maybe_tp().is_none()
            };
            if missing_tp {
                self.link_types(node_id).unwrap();
            }
        }
        if blockers.is_empty() {
            let pending_refs = mem::replace(&mut self.pending_refs, Vec::new());
            LinkerResult::Complete(pending_refs)
        } else {
            self.queue.extend(blockers.values().flatten());
            if made_progress {
                return LinkerResult::BlockedByType { blocked: blockers };
            } else {
                return LinkerResult::FailedToMakeProgress { blocked: blockers };
            }
        }
    }

    pub fn link(
        &mut self,
        node_id: NodeID,
        unlinked_body: UnlinkedNodeBody<T>,
    ) -> result::Result<LinkedNodeBody<T>, LinkerErr> {
        let linked_body = match unlinked_body {
            VariableAssignment { ident, path, expr } => {
                let (variable, location, _) = match self.closest_variable(node_id, &ident)? {
                    Some(node_id) => node_id,
                    None => Err(Err::single(
                        "Failed to find variable to assign to",
                        "variable not found",
                        vec![node_id],
                    ))?,
                };
                self.ast_mut()
                    .add_ref((variable, WriteValue), (expr, ReadValue), location);
                LinkedNodeBody::VariableAssignment {
                    variable,
                    path,
                    expr,
                }
            }
            StaticAssignment { ident, path, expr } => {
                let expr = expr;
                let (variable, location, variable_depth) =
                    match self.closest_variable(node_id, &ident)? {
                        Some(node_id) => node_id,
                        None => Err(Err::single(
                            "Failed to find type to assign to",
                            "type not found",
                            vec![node_id],
                        ))?,
                    };
                if let Some(ref associated_path) = path {
                    if variable_depth > 1 {
                        println!("{}", variable_depth);
                        Err(Err::single(
                            "Associated methods can only be defined in the same scope as type",
                            "Not allowed",
                            vec![node_id, expr],
                        ))?
                    }
                    if associated_path.len() != 1 {
                        Err(ast::Err::single(
                            "Invalid path for constant assignment (unimplemented)",
                            "Assignment of constant value",
                            vec![node_id],
                        ))?
                    };
                    let first_path = &associated_path[0];
                    let mut ast = self.ast_mut();
                    let declaration = match &mut ast.get_node_mut(variable).body {
                        PartialNodeBody::Linked(LinkedNodeBody::TypeDeclaration(td)) => td,
                        _ => Err(Err::single(
                            "Only allowed to statically assign to type declarations",
                            "Not allowed to be static",
                            vec![node_id],
                        ))?,
                    };
                    if declaration.methods.contains_key(first_path) {
                        Err(ast::Err::single(
                            "Method already defined on type",
                            "Assignment of constant value",
                            vec![node_id],
                        ))?
                    }
                    declaration.methods.insert(first_path.into(), expr);

                    ast.add_ref((variable, WriteValue), (expr, ReadValue), location);
                    LinkedNodeBody::Block {
                        static_body: vec![],
                        import_body: vec![],
                        dynamic_body: vec![],
                    }
                } else {
                    self.ast_mut()
                        .add_ref((variable, WriteValue), (expr, ReadValue), location);
                    LinkedNodeBody::ConstAssignment {
                        ident: variable,
                        path,
                        expr,
                    }
                }
            }
            Value { value, tp } => {
                let value = self.resolve_value(node_id, value)?;
                LinkedNodeBody::ConstValue { tp, value }
            }
            VariableValue { ident, path } => {
                let (variable, location, _) = match self.closest_variable(node_id, &ident)? {
                    Some(target) => target,
                    None => Err(Err::single(
                        "Failed to find variable",
                        "variable not found",
                        vec![node_id],
                    ))?,
                };
                self.ast_mut()
                    .add_ref((variable, ReadValue), (node_id, WriteValue), location);
                LinkedNodeBody::VariableValue { variable, path }
            }
            Call { ident, args, path } => {
                let (func, location, _) = match self.closest_variable(node_id, &ident)? {
                    Some(node_id) => node_id,
                    None => Err(Err::single(
                        "Failed to find variable to call",
                        "function not found",
                        vec![node_id],
                    ))?,
                };
                self.ast_mut()
                    .add_ref((func, GoTo), (node_id, ExecuteValue), location);
                if let Some(path) = path {
                    let ast = self.ast();
                    let body = &ast.get_node(func).body;
                    let declaration = match body {
                        PartialNodeBody::Linked(LinkedNodeBody::TypeDeclaration(t)) => t,
                        PartialNodeBody::Linked(LinkedNodeBody::VariableDeclaration {
                            tp, ..
                        }) => {
                            if let Some(tp) = tp {
                                let body = &ast.get_node(*tp).body;
                                match body {
                                    PartialNodeBody::Linked(LinkedNodeBody::TypeDeclaration(t)) => {
                                        t
                                    }
                                    _ => unimplemented!(),
                                }
                            } else {
                                return Err(LinkerErr::BlockedByNodeType(func));
                            }
                        }
                        _ => Err(Err::single(
                            "Can only call associated functions on types",
                            "invalid function call",
                            vec![node_id],
                        ))?,
                    };
                    if path.len() != 1 {
                        Err(Err::single(
                            "Can only call associated functions on non nested types",
                            "invalid function call",
                            vec![node_id],
                        ))?
                    }
                    let path = &path[0];
                    if let Some(static_fn) = declaration.methods.get(path) {
                        LinkedNodeBody::Call(NBCall {
                            func: *static_fn,
                            args,
                        })
                    } else {
                        Err(Err::single(
                            "Method not found on type",
                            "method not found",
                            vec![node_id],
                        ))?
                    }
                } else {
                    LinkedNodeBody::Call(NBCall { func, args })
                }
            }
            ImportValue { is_relative, path } => {
                if is_relative {
                    let parent = {
                        let ast = self.ast();
                        match ast.path.not_last() {
                            Some(parent_path) => match self.exports.get(parent_path) {
                                Some(NamespaceElement::Namespace(n)) => n,
                                _ => unimplemented!(),
                            },
                            None => &self.exports,
                        }
                    };
                    match parent.get(path.as_ref()) {
                        Some(NamespaceElement::Namespace(_)) => unimplemented!(),
                        Some(NamespaceElement::BuiltIn(_)) => unimplemented!(),
                        Some(NamespaceElement::Export(export)) => {
                            let export = *export;
                            if export.is_static {
                                self.pending_refs.push(PendingRef {
                                    target: (export.node_id, NodeReferenceType::ReadExternalValue),
                                    referencer: (node_id, NodeReferenceType::WriteExternalValue),
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
                let (func, location, _) = self.closest_fn(node_id)?;
                self.ast_mut()
                    .add_ref((func, GoTo), (node_id, ControlFlow), location);
                LinkedNodeBody::Return {
                    func,
                    expr,
                    automatic,
                }
            }
            Break => {
                let (r#loop, location, _) = self.closest_loop(node_id)?;
                self.ast_mut()
                    .add_ref((r#loop, GoTo), (node_id, ControlFlow), location);
                LinkedNodeBody::Break { r#loop }
            }
        };
        Ok(linked_body)
    }
}
