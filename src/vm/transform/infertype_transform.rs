use crate::vm;
use crate::vm::ast;
use crate::vm::ast::{
    Ast, AstBranch, AstBranchID, ErrPart, InferredType, IsValid, LNBTypeDeclaration,
    LinkedNodeBody, Node, NodeID, NodeType, NodeTypeSource, NodeValue, PartialNodeBody,
    PartialNodeValue, TypesInferred,
};
use crate::vm::ast::{NBCall, NBProcedureDeclaration};
use crate::vm::runtime::{Namespace, NamespaceElement, RuntimeDefinitions};
use crate::vm::transform::link_transform::Linker;
use crate::vm::transform::{AstTransformation, Result};
use std::collections::{HashMap, HashSet, VecDeque};
use std::fmt::{Debug, Formatter};
use std::result;
use std::sync::{Arc, RwLockReadGuard, RwLockWriteGuard};
use tokio::sync::mpsc::UnboundedSender;

struct Msg<T>
where
    T: IsValid,
{
    // linker: Linker<T>,
    typer: Typer<T>,
    tx: Box<UnboundedSender<Msg<T>>>,
    state: TyperState,
}

impl<T> Debug for Msg<T>
where
    T: IsValid,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let t = &self.typer;
        match &self.state {
            TyperState::Link => {
                unimplemented!();
                write!(f, "Link({:?}, q:{})", t.ast_id, t.queue.len())
            }
            TyperState::TypeCheck => {
                write!(f, "TypeCheck({:?}, q:{})", t.ast_id, t.queue.len())
            }
            TyperState::TypeCheckBlocked(ast_id) => {
                write!(f, "TypeCheckBlocked({:?}, by: {:?})", t.ast_id, ast_id)
            }
            TyperState::Err(err) => write!(f, "Err({})", err.to_string()),
            TyperState::Done(ast_id) => write!(f, "Done({:?})", ast_id),
        }
    }
}

impl<T> Msg<T>
where
    T: IsValid,
{
    fn mut_state(mut self, state: TyperState) -> Self {
        self.state = state;
        self
    }

    fn send(mut self, state: TyperState) {
        self.state = state;
        self.resend()
    }

    fn resend(self) {
        let raw = &*self.tx as *const UnboundedSender<_>;
        // Safety: The value sent through tx always lives longer than the send call to the tx channel.
        let tx = unsafe { &*raw };
        tx.send(self).unwrap()
    }
}

enum TyperState {
    Link,
    TypeCheck,
    TypeCheckBlocked(AstBranchID),
    Err(ast::Err),
    Done(AstBranchID),
}

enum TyperResult {
    Complete,
    BlockedBy {
        blocked_by: AstBranchID,
    },
    FailedToMakeProgress {
        blocked: HashMap<NodeID, Vec<NodeID>>,
    },
    Failed(ast::Err),
}

pub struct InferTypes<'a> {
    tokio_runtime: &'a tokio::runtime::Runtime,
    vm_runtime: Arc<RuntimeDefinitions>,
}

impl<'a> InferTypes<'a> {
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

impl<'a, T> AstTransformation<T, TypesInferred> for InferTypes<'a>
where
    T: IsValid,
{
    fn name(&self) -> String {
        "Infer Types".to_string()
    }
    fn transform(&self, ast: Ast<T>) -> Result<TypesInferred> {
        self.tokio_runtime.block_on(async {
            let (tx, mut rx) = tokio::sync::mpsc::unbounded_channel();
            let mut n_active = 0;
            let mut n_blocked = 0;

            let mut exports = Namespace::new();
            for (path, ast) in ast.paths() {
                exports.set(path.as_ref(), NamespaceElement::Namespace(ast.exports()));
            }

            let exports = Arc::new(exports);

            let asts = Arc::new(ast);
            for id in (&asts).ids() {
                n_active += 1;
                Msg {
                    // linker: Linker::new(id, self.vm_runtime.clone(), exports.clone()),
                    typer: Typer::new(id, asts.clone(), self.vm_runtime.clone()),
                    tx: Box::new(tx.clone()),
                    state: TyperState::TypeCheck,
                }.resend();
            }

            let mut blocked_checkers = HashMap::new();
            let mut completed = HashSet::new();
            let mut error = None;

            drop(tx);
            while let Some(mut msg) = rx.recv().await {
                if error.is_some() {
                    n_active -= 1;
                    continue
                }
                match msg.state {
                    TyperState::Link => {
                        let vm_runtime = self.vm_runtime.clone();
                        let exports = exports.clone();
                        tokio::task::spawn_blocking(move || {
                            // let mut ast = asts.write_ast(msg.typer.ast_id);
                            // let linker = Linker::new(&mut ast, vm_runtime, exports);
                            // match linker.link() {
                            //     Ok(pending) => Ok(pending),
                            //     Err(e) => Err(e),
                            // }
                            unimplemented!()
                        });
                    },
                    TyperState::TypeCheck => {
                        tokio::task::spawn_blocking(move || {
                            use TyperResult::*;
                            let ast_id = msg.typer.ast_id;
                            match msg.typer.infer_all_types() {
                                Complete => msg.send(TyperState::Done(ast_id)),
                                BlockedBy { blocked_by } => msg.send(TyperState::TypeCheckBlocked(blocked_by)),
                                FailedToMakeProgress { blocked } => {
                                    let err = ast::Err::single(
                                        &format!("Unable to make progress type checking"), 
                                        "",
                                        blocked.values().flatten().cloned().collect()
                                    );
                                    msg.send(TyperState::Err(err))
                                },
                                Failed(err) => msg.send(TyperState::Err(err)),
                            };
                        });
                    }
                    TyperState::TypeCheckBlocked(by) => {
                        let msg = msg.mut_state(TyperState::TypeCheck);
                        if completed.contains(&by) {
                            msg.resend();
                        } else {
                            n_blocked += 1;
                            if !blocked_checkers.contains_key(&by) {
                                blocked_checkers.insert(by, vec![]);
                            }
                            blocked_checkers.get_mut(&by).unwrap().push(msg);
                        }
                    }
                    TyperState::Err(err) => {
                        n_active -= 1;
                        error = Some(err);
                    }
                    TyperState::Done(ast_id) => {
                        n_active -= 1;
                        completed.insert(ast_id);
                        if let Some(blocked) = blocked_checkers.get_mut(&ast_id) {
                            let blocked = std::mem::replace(blocked, vec![]);
                            for msg in blocked {
                                n_blocked -= 1;
                                msg.resend();
                            }
                        }
                    }
                }
                if n_active > 0 && n_blocked == n_active {
                    let nodes = blocked_checkers
                        .values()
                        .flatten()
                        .map(|msg| msg.typer.queue.iter())
                        .flatten()
                        .cloned()
                        .collect();
                    error = Some(ast::Err::single(
                        "Failed to complete type check, unable to infer the type of the following expressions",
                        "unable to infer type",
                        nodes,
                    ));
                }
            }

            // Drop blocked_checkers to ensure that all other references to the arch are dropped.
            drop(blocked_checkers);
            let asts = Arc::try_unwrap(asts).expect("Single instance of ast in infer type transform error");
            return if let Some(err) = error {
                Err((asts.guarantee_state(), err))
            } else {
                Ok(asts.guarantee_state())
            }
        })
    }
}

enum TypeInferenceErr {
    BlockedByNodeType(NodeID),
    BlockedByUnlinkedNode(NodeID),
    Fail(ast::Err),
}

type TypeInferenceResult<T = InferredType> = result::Result<T, TypeInferenceErr>;

pub struct Typer<T>
where
    T: IsValid,
{
    queue: VecDeque<NodeID>,
    ast_id: AstBranchID,
    asts: Arc<Ast<T>>,
    runtime: Arc<vm::runtime::RuntimeDefinitions>,
}

impl<T> Debug for Typer<T>
where
    T: IsValid,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Typer")
    }
}

impl<T> Typer<T>
where
    T: IsValid,
{
    pub fn new(
        ast_id: AstBranchID,
        asts: Arc<Ast<T>>,
        runtime: Arc<vm::runtime::RuntimeDefinitions>,
    ) -> Self {
        let root = asts.read_ast(ast_id).root();
        let queue = VecDeque::from(vec![root]);
        Self {
            queue,
            ast_id,
            asts,
            runtime,
        }
    }

    fn coerce_types<'a, 'b: 'a>(
        &self,
        ast: &'b RwLockReadGuard<AstBranch<T>>,
        lhs: NodeID,
        rhs: NodeID,
    ) -> TypeInferenceResult<&'a NodeType> {
        let lhs_tp = self.get_type(ast, lhs)?;
        let rhs_tp = self.get_type(ast, rhs)?;
        if lhs_tp == rhs_tp {
            Ok(lhs_tp)
        } else {
            Err(TypeInferenceErr::Fail(ast::Err::single(
                "Left and right hand sides do not match",
                "",
                vec![lhs, rhs],
            )))
        }
    }

    fn get_type_node_void<'a, 'b: 'a>(
        &self,
        ast: &'b RwLockReadGuard<AstBranch<T>>,
        node_id: NodeID,
    ) -> TypeInferenceResult<&'a NodeType> {
        match self.get_type(ast, node_id)? {
            NodeType::Void => Err(TypeInferenceErr::Fail(ast::Err::single(
                "Did not expect void value here",
                "value is void",
                vec![node_id],
            ))),
            tp => Ok(tp),
        }
    }

    fn ast(&self) -> RwLockReadGuard<AstBranch<T>> {
        self.asts.read_ast(self.ast_id)
    }

    fn ast_mut(&self) -> RwLockWriteGuard<AstBranch<T>> {
        self.asts.write_ast(self.ast_id)
    }

    fn get_type<'a, 'b: 'a>(
        &self,
        ast: &'b RwLockReadGuard<AstBranch<T>>,
        node_id: NodeID,
    ) -> TypeInferenceResult<&'a NodeType> {
        let node_id = node_id;
        if let Some(tp) = ast.get_node(node_id).maybe_tp() {
            Ok(tp)
        } else {
            Err(TypeInferenceErr::BlockedByNodeType(node_id))
        }
    }

    fn node_value_type(&self, value: &PartialNodeValue<T>) -> Option<NodeType> {
        let value = match value {
            PartialNodeValue::Linked(value) => value,
            PartialNodeValue::Unlinked(_) => return None,
            PartialNodeValue::_TP(_) => unreachable!(),
        };
        let tp = match value {
            NodeValue::Int(..) => NodeType::Int,
            NodeValue::Float(..) => NodeType::Float,
            NodeValue::String(..) => NodeType::String,
            NodeValue::Bool(..) => NodeType::Bool,
            NodeValue::Struct(fields) => {
                let types = fields
                    .iter()
                    .map(|(_, value)| self.node_value_type(&*value))
                    .collect::<Vec<Option<NodeType>>>();
                if types.iter().any(|tp| tp.is_none()) {
                    return None;
                }
                let field_types = fields
                    .iter()
                    .map(|(name, _)| name)
                    .zip(types.into_iter())
                    .map(|(name, tp)| (name.clone(), tp.unwrap()))
                    .collect();
                NodeType::Struct {
                    fields: field_types,
                }
            }
            NodeValue::RuntimeFn(id) => {
                let def = &self.runtime;
                let path = &def.paths[*id];
                def.namespace.get_tp(path.as_ref()).unwrap().clone()
            }
        };
        Some(tp)
    }

    fn infer_type(&self, node: &Node<T>) -> result::Result<InferredType, TypeInferenceErr> {
        use crate::vm::ast::LinkedNodeBody::*;
        use crate::vm::ast::NodeType::*;
        use crate::vm::ast::NodeTypeSource::*;
        use TypeInferenceErr::*;
        let ast = self.ast();

        let body = match &node.body {
            PartialNodeBody::Linked(body) => body,
            PartialNodeBody::Unlinked(_) => return Err(BlockedByUnlinkedNode(node.id())),
            PartialNodeBody::Empty => unreachable!(),
        };

        match body {
            TypeReference { tp } => match ast.partial_type(*tp) {
                Some((_, tp)) => Ok(InferredType::new(tp.clone(), NodeTypeSource::Declared)),
                None => Err(Fail(ast::Err::single(
                    "Invalid type reference",
                    "",
                    vec![node.id()],
                ))),
            },
            PartialType { tp, parts } => match tp.option() {
                Some(tp) => Ok(InferredType::new(tp.clone(), NodeTypeSource::Declared)),
                None => {
                    let mut nodes = vec![node.id()];
                    nodes.append(&mut parts.clone());
                    return Err(Fail(ast::Err::single("Incomplete partial type", "", nodes)));
                }
            },
            ConstValue { value, tp } => {
                let tp = match tp {
                    Some(tp_id) => match ast.partial_type(*tp_id) {
                        Some((_, tp)) => Some(tp.clone()),
                        None => None,
                    },
                    None => None,
                };
                let tp = match tp {
                    Some(tp) => tp,
                    None => match self.node_value_type(&*value) {
                        None => return Err(BlockedByUnlinkedNode(node.id())),
                        Some(tp) => tp,
                    },
                };
                Ok(InferredType::new(tp, Declared))
            }
            Op { op, lhs, rhs } => {
                use crate::vm::ast::ArithmeticOP::*;
                match op {
                    GEq | LEq | Eq => Ok(InferredType::new(Bool, Declared)),
                    Add | Sub | Mul | Div => {
                        let tp = self.coerce_types(&ast, *lhs, *rhs)?;
                        Ok(InferredType::new(tp.clone(), Value))
                    }
                }
            }
            ProcedureDeclaration(NBProcedureDeclaration { args, returns, .. }) => {
                let mut arg_types = Vec::with_capacity(args.len());
                for id in args {
                    let tp = self.get_type(&ast, *id)?;
                    arg_types.push(tp.clone());
                }
                let return_type = match *returns {
                    Some(returns) => self.get_type(&ast, returns)?.clone(),
                    None => Void,
                };
                Ok(InferredType::new(
                    Fn {
                        args: arg_types,
                        returns: Box::new(return_type),
                    },
                    Declared,
                ))
            }
            PrefixOp { rhs, .. } => {
                let tp = self.get_type(&ast, *rhs)?;
                Ok(InferredType::new(tp.clone(), Value))
            }
            Reference { node_id } => {
                drop(ast);
                let node = (&self.asts).get_node(*node_id);
                match node.maybe_inferred_tp() {
                    Some(tp) => Ok(tp.clone()),
                    None => Err(BlockedByNodeType(*node_id)),
                }
            }
            VariableValue { variable, path } => {
                let mut value_tp = self.get_type(&ast, *variable)?;
                if let NodeType::NewType { .. } = value_tp {
                    let node = ast.get_node(*variable);
                    let body = match &node.body {
                        PartialNodeBody::Linked(body) => body,
                        PartialNodeBody::Unlinked(_) => unimplemented!(),
                        PartialNodeBody::Empty => unreachable!(),
                    };
                    match body {
                            LinkedNodeBody::ConstDeclaration { expr, .. }
                            | LinkedNodeBody::StaticDeclaration { expr, .. } => {
                                value_tp = self.get_type(&ast, *expr)?;
                            }
                            LinkedNodeBody::TypeDeclaration(LNBTypeDeclaration { ident, .. }) => {
                                Err(Fail(ast::Err::single(
                                    "When instantiating a type, use the default instantiation function by adding ()",
                                    &format!("Replace with {}()", ident),
                                    vec![node.id()],
                                )))?
                            }
                            _ => unreachable!("{:?}", *variable),
                        }
                }
                if let Some(path) = path {
                    for path_field in path {
                        let fields = if let NodeType::Struct { fields } = &value_tp {
                            fields
                        } else if let NodeType::Type { content, .. } = &value_tp {
                            if let NodeType::Struct { fields } = &**content {
                                fields
                            } else {
                                unimplemented!("{:?}", value_tp)
                            }
                        } else {
                            unimplemented!("{:?}", value_tp)
                        };
                        let mut tp = None;
                        for (field, field_tp) in fields {
                            if path_field == field {
                                tp = Some(field_tp);
                                break;
                            }
                        }
                        match tp {
                            Some(tp) => {
                                value_tp = tp;
                            }
                            None => unreachable!(),
                        }
                    }
                }
                Ok(InferredType::new(value_tp.clone(), Value))
            }
            Expression(value) => {
                let tp = self.get_type(&ast, *value)?;
                Ok(InferredType::new(tp.clone(), Value))
            }
            VariableDeclaration { tp, expr, .. } => {
                if let Some(tp) = tp {
                    let tp = self.get_type(&ast, *tp)?;
                    Ok(InferredType::new(tp.clone(), Declared))
                } else if let Some(expr) = expr {
                    let tp = self.get_type_node_void(&ast, *expr)?;
                    Ok(InferredType::new(tp.clone(), Value))
                } else {
                    unreachable!()
                }
            }
            ConstDeclaration { tp, expr, .. } | StaticDeclaration { tp, expr, .. } => {
                if let Some(tp) = tp {
                    let tp = self.get_type(&ast, *tp)?;
                    Ok(InferredType::new(tp.clone(), Declared))
                } else {
                    let tp = self.get_type_node_void(&ast, *expr)?;
                    Ok(InferredType::new(tp.clone(), Value))
                }
            }
            TypeDeclaration(LNBTypeDeclaration { tp, .. }) => match ast.get_node_type(*tp) {
                Some(tp) => Ok(InferredType::new(tp.clone(), Declared)),
                None => Err(BlockedByNodeType(*tp)),
            },
            Import { expr, .. } => {
                let tp = self.get_type(&ast, *expr)?;
                Ok(InferredType::new(tp.clone(), Value))
            }
            VariableAssignment { .. } => Ok(InferredType::new(Void, Declared)),
            ConstAssignment { .. } => Ok(InferredType::new(Void, Declared)),
            Call(NBCall { func, .. }) => {
                let tp = self.get_type(&ast, *func)?;
                match tp {
                        Fn { returns, .. } | NewType { tp: returns, .. } => {
                            Ok(InferredType::new((**returns).clone(), Value))
                        }
                        tp => Err(Fail(ast::Err::new(
                            format!(
                                "It's currently only possible to call functions, tried to call {:?}: {:?}",
                                *func,
                                tp
                            ),
                            vec![
                                ErrPart::new("Calling non-callable variable".to_string(), vec![node.id()]),
                                ErrPart::new("Non-callable variable is defined here".to_string(), vec![*func])
                            ]
                        ))),
                    }
            }
            Break { .. } | Return { .. } | Block { .. } | If { .. } | Loop { .. } | Comment(..) => {
                Ok(InferredType::new(Void, Declared))
            }
        }
    }

    fn infer_all_types(&mut self) -> TyperResult {
        use TypeInferenceErr::*;

        let mut blockers = HashMap::new();

        let mut made_progress = false;
        let ast_id = self.ast_id;
        while let Some(node_id) = self.queue.pop_front() {
            let tp = {
                let ast = self.asts.read_ast(self.ast_id);
                // let ast = self.ast();
                let node = ast.get_node(node_id);
                for &child_id in node.body.children() {
                    if let None = ast.get_node(child_id).maybe_tp() {
                        self.queue.push_back(child_id)
                    }
                }
                self.infer_type(node)
            };

            match tp {
                Ok(tp) => {
                    made_progress = true;
                    {
                        let mut ast = self.ast_mut();
                        let node = ast.get_node_mut(node_id);
                        node.set_inferred_tp(tp);
                    }
                    if let Some(blocked) = blockers.remove(&node_id) {
                        for blocked_id in blocked {
                            self.queue.push_back(blocked_id);
                        }
                    }
                }
                Err(BlockedByNodeType(blocking_id)) => {
                    if !blockers.contains_key(&blocking_id) {
                        blockers.insert(blocking_id, vec![]);
                    }
                    blockers.get_mut(&blocking_id).unwrap().push(node_id);
                }
                Err(BlockedByUnlinkedNode(blocking_id)) => {
                    unimplemented!()
                }
                Err(Fail(err)) => return TyperResult::Failed(err),
            };
        }

        if blockers.is_empty() {
            TyperResult::Complete
        } else {
            self.queue.extend(blockers.values().flatten());
            for (blocking_id, _) in blockers.iter() {
                if blocking_id.ast() != ast_id {
                    if made_progress {
                        return TyperResult::BlockedBy {
                            blocked_by: blocking_id.ast(),
                        };
                    } else {
                        return TyperResult::FailedToMakeProgress { blocked: blockers };
                    }
                }
            }
            let parts = self
                .queue
                .iter()
                .map(|n| ErrPart::new("Unable to resolve type".to_string(), vec![*n]))
                .collect();
            TyperResult::Failed(ast::Err::new(
                "Unable to make progress type checking...".to_string(),
                parts,
            ))
        }
    }
}
