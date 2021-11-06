use crate::ast;
use crate::ast::ast::{AstCollection, InferredType, Linked, StateTypesInferred};
use crate::ast::nodebody::{NBCall, NBProcedureDeclaration, NodeBody};
use crate::ast::{Ast, AstID, ErrPart, Node, NodeID, NodeType, NodeTypeSource, NodeValue, Result};
use crate::runtime::{FunctionDefinition, Runtime};
use crate::token::ArithmeticOP;
use std::borrow::Borrow;
use std::collections::{HashMap, VecDeque};
use std::fmt::{Debug, Formatter};
use std::result;
use std::sync::{Arc, RwLockReadGuard, RwLockWriteGuard};

enum TyperState<T>
where
    T: Linked + 'static,
{
    TypeCheck(Typer<T>),
    TypeCheckBlocked(AstID, Typer<T>),
    Err(ast::Err),
    Done(AstID),
}

impl<T> Debug for TyperState<T>
where
    T: Linked + 'static,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TyperState::TypeCheck(t) => write!(f, "TypeCheck({:?}, q:{})", t.ast_id, t.queue.len()),
            TyperState::TypeCheckBlocked(ast_id, t) => {
                write!(f, "TypeCheckBlocked({:?}, by: {:?})", t.ast_id, ast_id)
            }
            TyperState::Err(err) => write!(f, "Err({})", err.details),
            TyperState::Done(ast_id) => write!(f, "Done({:?})", ast_id),
        }
    }
}

pub fn infer_types<T>(
    asts: AstCollection<T>,
    vm_runtime: &Runtime,
    runtime: &tokio::runtime::Runtime,
) -> result::Result<AstCollection<StateTypesInferred>, (AstCollection<T>, ast::Err)>
where
    T: Linked + 'static,
{
    runtime.block_on(async {
        let (tx, mut rx) = tokio::sync::mpsc::unbounded_channel();
        let mut n_active = 0;
        let asts = Arc::new(asts);
        for id in (&asts).iter_keys() {
            let asts = asts.clone();
            let definitions = vm_runtime.definitions.clone();
            let typer = Typer::new(id, asts, definitions);
            tx.send(TyperState::TypeCheck(typer)).unwrap();
            n_active += 1;
        }
        let mut since_last_changed = 0;

        let mut blocked_checkers = HashMap::new();
        let mut n_blocked = 0;

        while let Some(msg) = rx.recv().await {
            println!("{:?}", msg);
            match msg {
                TyperState::TypeCheck(mut typer) => {
                    since_last_changed += 1;
                    let tx = tx.clone();
                    tokio::task::spawn_blocking(move || {
                        match typer.infer_all_types() {
                            Err(err) => tx.send(TyperState::Err(err)).unwrap(),
                            Ok(Some(blocked_by)) => tx.send(TyperState::TypeCheckBlocked(blocked_by, typer)).unwrap(),
                            Ok(None) => tx.send(TyperState::Done(typer.ast_id)).unwrap(),
                        };
                    });
                }
                TyperState::TypeCheckBlocked(by, typer) => {
                    if !blocked_checkers.contains_key(&by) {
                        blocked_checkers.insert(by, vec![]);
                    }
                    blocked_checkers.get_mut(&by).unwrap().push(typer);
                    n_blocked += 1;
                }
                TyperState::Err(err) => {
                    let asts = Arc::try_unwrap(asts).unwrap();
                    return Err((asts, err));
                }
                TyperState::Done(ast_id) => {
                    since_last_changed = 0;
                    n_active -= 1;
                    if let Some(blocked) = blocked_checkers.get_mut(&ast_id) {
                        let blocked = std::mem::replace(blocked, vec![]);
                        for typer in blocked {
                            tx.send(TyperState::TypeCheck(typer)).unwrap();
                            n_blocked -= 1;
                        }
                    }
                }
            }
            if n_active == 0 {
                break;
            }
            if since_last_changed > n_active || n_blocked == n_active {
                let nodes = blocked_checkers
                    .into_values()
                    .map(|typers|typers
                        .into_iter()
                        .map(|typer|Vec::from(typer.queue))
                        .reduce(|mut all, mut next|{
                            all.append(&mut next);
                            all
                        })
                        .unwrap_or(vec![])
                    )
                    .reduce(|mut all, mut next| {
                        all.append(&mut next);
                        all
                    })
                    .unwrap_or(vec![]);
                let err = ast::Err::single(
                    "Failed to complete type check, unable to infer the type of the following expressions",
                    "unable to infer type",
                    nodes,
                );
                let asts = Arc::try_unwrap(asts).unwrap();
                return Err((asts, err));
            }
        }

        let asts = Arc::try_unwrap(asts).unwrap();
        Ok(asts.guarantee_state())
    })
}

#[derive(Debug)]
pub struct Typer<T>
where
    T: Linked,
{
    queue: VecDeque<NodeID>,
    blocked: HashMap<NodeID, Vec<NodeID>>,
    ast_id: AstID,
    asts: Arc<AstCollection<T>>,
    runtime: Arc<Vec<FunctionDefinition>>,
    since_last_changed: usize,
}

impl<T> Typer<T>
where
    T: Linked,
{
    pub fn new(
        ast_id: AstID,
        asts: Arc<AstCollection<T>>,
        runtime: Arc<Vec<FunctionDefinition>>,
    ) -> Self {
        let root = asts.get(ast_id).read().unwrap().root();
        let queue = VecDeque::from(vec![root]);
        let since_last_changed = 0;
        Self {
            queue,
            ast_id,
            asts,
            runtime,
            since_last_changed,
        }
    }

    fn try_coerce(&self, tp1_opt: Option<NodeType>, tp2_opt: Option<NodeType>) -> Option<NodeType> {
        match (&tp1_opt, &tp2_opt) {
            (Some(tp1), Some(tp2)) => {
                if tp1 == tp2 {
                    return tp1_opt;
                }
                match (tp1, tp2) {
                    (NodeType::NotYetImplemented, _) => tp2_opt,
                    (_, NodeType::NotYetImplemented) => tp1_opt,
                    _ => None,
                }
            }
            _ => None,
        }
    }

    fn not_void(&self, node_id: &NodeID) -> Result<()> {
        let tp = self.get_type(node_id);
        match tp {
            Some(NodeType::Void) => Err(ast::Err::single(
                "Did not expect void value here",
                "value is void",
                vec![*node_id],
            )),
            _ => Ok(()),
        }
    }

    fn ast(&self) -> RwLockReadGuard<Ast<T>> {
        self.asts.get(self.ast_id).read().unwrap()
    }

    fn ast_mut(&self) -> RwLockWriteGuard<Ast<T>> {
        self.asts.get(self.ast_id).write().unwrap()
    }

    fn get_type(&self, node_id: &NodeID) -> Option<NodeType> {
        let node_id = *node_id;
        let ast = self.ast();
        if let Some(tp) = ast.get_node(node_id).maybe_tp() {
            Some(tp.clone())
        } else if let Some((_, tp)) = ast.borrow().partial_type(node_id) {
            Some(tp.clone())
        } else {
            None
        }
    }

    fn node_value_type(&self, value: &NodeValue<T>) -> NodeType {
        match value {
            NodeValue::Int(..) => NodeType::Int,
            NodeValue::Float(..) => NodeType::Float,
            NodeValue::String(..) => NodeType::String,
            NodeValue::Bool(..) => NodeType::Bool,
            NodeValue::Struct(fields) => {
                let field_types = fields
                    .iter()
                    .map(|(name, value)| {
                        let tp = self.node_value_type(value.into());
                        (name.clone(), tp)
                    })
                    .collect();
                NodeType::Struct {
                    fields: field_types,
                }
            }
            NodeValue::RuntimeFn(id) => {
                let def: &Vec<FunctionDefinition> = &self.runtime.borrow();
                def[*id].tp.clone()
            }
        }
    }

    pub fn infer_type(&self, node: &Node<T>) -> Result<Option<InferredType>> {
        use super::NodeType::*;
        use super::NodeTypeSource::*;
        use crate::ast::nodebody::NodeBody::*;
        let ast = self.ast();
        let tp = match &node.body {
            TypeReference { tp } => ast
                .partial_type(*tp)
                .map(|(_, tp)| InferredType::new(tp.clone(), NodeTypeSource::Declared)),
            PartialType { tp, .. } => tp
                .option()
                .map(|tp| InferredType::new(tp.clone(), NodeTypeSource::Declared)),
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
                    None => self.node_value_type(value.into()),
                };
                Some(InferredType::new(tp, Declared))
            }
            Op { op, lhs, rhs } => {
                use ArithmeticOP::*;
                match op {
                    GEq | LEq | Eq => InferredType::maybe(Some(Bool), Declared),
                    Add | Sub | Mul | Div => {
                        let lhs = self.get_type(lhs);
                        let rhs = self.get_type(rhs);
                        let tp = self.try_coerce(lhs, rhs);
                        InferredType::maybe(tp, Value)
                    }
                }
            }
            ProcedureDeclaration(NBProcedureDeclaration { args, returns, .. }) => {
                let arg_types: Vec<Option<NodeType>> =
                    args.iter().map(|id| self.get_type(id)).collect();
                let args_inferred = arg_types.iter().all(|tp| tp.is_some());
                if args_inferred {
                    let arg_types = arg_types.into_iter().map(|tp| tp.unwrap()).collect();
                    let return_type = match *returns {
                        Some(returns) => ast.get_node(returns).maybe_tp().cloned(),
                        None => Some(Void),
                    };
                    if let Some(return_type) = return_type {
                        InferredType::maybe(
                            Some(Fn {
                                args: arg_types,
                                returns: Box::new(return_type),
                            }),
                            Declared,
                        )
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            PrefixOp { rhs, .. } => InferredType::maybe(self.get_type(rhs), Value),
            Reference { node_id } => {
                drop(ast);
                let node = (&self.asts).get_node(*node_id);
                node.maybe_inferred_tp().cloned()
            }
            VariableValue { variable, path } => {
                if let Some(value_tp) = self.get_type(variable) {
                    let mut value_tp = value_tp;
                    if let NodeType::NewType { .. } = value_tp {
                        let body = &ast.get_node(*variable).body;
                        match body {
                            NodeBody::ConstDeclaration{expr, ..}
                            | NodeBody::StaticDeclaration{expr, ..}=> {
                                value_tp = self.get_type(expr).unwrap();
                            }
                            NodeBody::TypeDeclaration{ ident, ..} => {
                                Err(ast::Err::single(
                                    "When instantiating a type, use the default instantiation function by adding ()",
                                    &format!("Replace with {}()", ident),
                                    vec![node.id()],
                                ))?
                            }
                            _ => unreachable!("{:?}", *variable),
                        }
                    }
                    let mut value_tp = &value_tp;
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
                    Some(InferredType::new(value_tp.clone(), Value))
                } else {
                    None
                }
            }
            Expression(value) => {
                let tp = self.infer_type(ast.get_node(*value))?;
                match tp {
                    Some(tp) => Some(InferredType::new(tp.tp, Value)),
                    None => None,
                }
            }
            VariableDeclaration { tp, expr, .. } => {
                if let Some(tp) = tp {
                    InferredType::maybe(ast.get_node_type(*tp), Declared)
                } else if let Some(expr) = expr {
                    self.not_void(expr)?;
                    InferredType::maybe(self.get_type(expr), Value)
                } else {
                    None
                }
            }
            ConstDeclaration { tp, expr, .. } | StaticDeclaration { tp, expr, .. } => {
                if let Some(tp) = tp {
                    InferredType::maybe(ast.get_node_type(*tp), Declared)
                } else if let Some(expr_tp) = self.get_type(expr) {
                    self.not_void(expr)?;
                    InferredType::maybe(Some(expr_tp), Value)
                } else {
                    None
                }
            }
            TypeDeclaration { tp, .. } => InferredType::maybe(ast.get_node_type(*tp), Declared),
            Import { expr, .. } => {
                if let Some(tp) = self.get_type(expr) {
                    InferredType::maybe(Some(tp), Value)
                } else {
                    None
                }
            }
            VariableAssignment { .. } => InferredType::maybe(Some(Void), Declared),
            Call(NBCall { func, .. }) => {
                let var = ast.get_node(*func);
                if let Some(tp) = var.maybe_tp() {
                    match tp {
                        Fn{returns, ..} |
                        NewType {tp: returns, .. }=> {
                            InferredType::maybe(Some((**returns).clone()), Value)
                        }
                        _ => Err(ast::Err::single(
                            &format!(
                                "It's currently only possible to call functions, tried to call {:?}",
                                var
                            ),
                            "",
                            vec![node.id(), *func],
                        ))?
                    }
                } else {
                    None
                }
            }
            Empty
            | Break { .. }
            | Return { .. }
            | Block { .. }
            | If { .. }
            | Loop { .. }
            | Comment(..) => InferredType::maybe(Some(Void), Declared),
            Unlinked(_) => unreachable!(),
        };
        Ok(tp)
    }

    pub fn infer_all_types(&mut self) -> Result<Option<AstID>> {
        let ast_id = self.ast_id;
        while let Some(node_id) = self.queue.pop_front() {
            let ast = self.asts.get(self.ast_id).read().unwrap();
            let node = ast.get_node(node_id);
            for &child_id in node.body.children() {
                if let None = ast.get_node(child_id).maybe_tp() {
                    self.queue.push_back(child_id)
                }
            }
            let tp = self.infer_type(node)?;
            drop(ast);
            if let Some(tp) = tp {
                let source = tp.source;
                let mut ast = self.ast_mut();
                let node = ast.get_node_mut(node_id);
                node.infer_type(tp);
                drop(ast);
                // Do not mark values which are only used as type checked
                if source != NodeTypeSource::Usage {
                    self.since_last_changed = 0;
                    continue;
                }
            }

            self.since_last_changed += 1;
            self.queue.push_back(node_id);
            if self.since_last_changed > self.queue.len() {
                let foreign_node_id = self.queue.iter().find(|n| n.ast() != ast_id);
                return match foreign_node_id {
                    Some(id) => {
                        self.since_last_changed = 0;
                        Ok(Some(id.ast()))
                    }
                    None => {
                        let parts = self
                            .queue
                            .iter()
                            .map(|n| ErrPart::new("Unable to resolve type".to_string(), vec![*n]))
                            .collect();
                        Err(ast::Err::new(
                            "Unable to make progress type checking...".to_string(),
                            parts,
                        ))
                    }
                };
            }
        }
        Ok(None)
    }
}
