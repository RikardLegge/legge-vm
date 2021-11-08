use crate::ast;
use crate::ast::ast::{AstCollection, InferredType, Linked, StateTypesInferred};
use crate::ast::nodebody::{NBCall, NBProcedureDeclaration, NodeBody};
use crate::ast::typer::TypeInferenceErr::Fail;
use crate::ast::{Ast, AstID, ErrPart, Node, NodeID, NodeType, NodeTypeSource, NodeValue};
use crate::runtime::{FunctionDefinition, Runtime};
use crate::token::ArithmeticOP;
use std::borrow::Borrow;
use std::collections::{HashMap, HashSet, VecDeque};
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
        let mut n_blocked = 0;

        let asts = Arc::new(asts);
        for id in (&asts).ids() {
            let asts = asts.clone();
            let definitions = vm_runtime.definitions.clone();
            let typer = Typer::new(id, asts, definitions);
            tx.send(TyperState::TypeCheck(typer)).unwrap();
            n_active += 1;
        }

        let mut blocked_checkers = HashMap::new();
        let mut completed = HashSet::new();
        let mut error = None;

        while let Some(msg) = rx.recv().await {
            if error.is_some() {
                n_active -= 1;
                if n_active == 0 {
                    drop(blocked_checkers);
                    let asts = Arc::try_unwrap(asts).unwrap();
                    let err = std::mem::replace(&mut error, None).unwrap();
                    return Err((asts, err));
                }
                continue
            }
            match msg {
                TyperState::TypeCheck(mut typer) => {
                    let tx = tx.clone();
                    tokio::task::spawn_blocking(move || {
                        use TyperResult::*;
                        match typer.infer_all_types() {
                            Complete => tx.send(TyperState::Done(typer.ast_id)).unwrap(),
                            BlockedBy { blocked_by, made_progress} => {
                                if made_progress {
                                    tx.send(TyperState::TypeCheckBlocked(blocked_by, typer)).unwrap()
                                } else {
                                    tx.send(TyperState::Err(ast::Err::single("Unable to make progress type checking...", "", typer.blocked.values().flatten().cloned().collect()))).unwrap()
                                }
                            },
                            Failed(err) => tx.send(TyperState::Err(err)).unwrap(),
                        };
                    });
                }
                TyperState::TypeCheckBlocked(by, typer) => {
                    if completed.contains(&by) {
                        tx.send(TyperState::TypeCheck(typer)).unwrap();
                    } else {
                        if !blocked_checkers.contains_key(&by) {
                            blocked_checkers.insert(by, vec![]);
                        }
                        blocked_checkers.get_mut(&by).unwrap().push(typer);
                        n_blocked += 1;
                    }
                }
                TyperState::Err(err) => {
                    error = Some(err);
                    n_active -= 1;
                }
                TyperState::Done(ast_id) => {
                    n_active -= 1;
                    if let Some(blocked) = blocked_checkers.get_mut(&ast_id) {
                        let blocked = std::mem::replace(blocked, vec![]);
                        for typer in blocked {
                            tx.send(TyperState::TypeCheck(typer)).unwrap();
                            n_blocked -= 1;
                        }
                    }
                    completed.insert(ast_id);
                }
            }
            if n_active == 0 {
                if blocked_checkers.values().any(|l|l.len()>0) {
                    panic!("Not all blockers are dropped")
                }
                let asts = Arc::try_unwrap(asts).unwrap();
                return Ok(asts.guarantee_state());
            }
            if n_blocked == n_active {
                let nodes = blocked_checkers
                    .into_values()
                    .map(|typers|typers
                        .into_iter()
                        .map(|typer|typer.blocked.values().flatten().cloned().collect::<Vec<NodeID>>())
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
        unreachable!()
    })
}

enum TypeInferenceErr {
    BlockedBy(NodeID),
    Fail(ast::Err),
}

type TypeInferenceResult<T = InferredType> = result::Result<T, TypeInferenceErr>;

pub struct Typer<T>
where
    T: Linked,
{
    queue: VecDeque<NodeID>,
    blocked: HashMap<NodeID, Vec<NodeID>>,
    ast_id: AstID,
    asts: Arc<AstCollection<T>>,
    runtime: Arc<Vec<FunctionDefinition>>,
}

impl<T> Debug for Typer<T>
where
    T: Linked,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Typer")
    }
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
        let root = asts.get(ast_id).root();
        let queue = VecDeque::from(vec![root]);
        let blocked = HashMap::new();
        Self {
            queue,
            blocked,
            ast_id,
            asts,
            runtime,
        }
    }

    fn coerce_types<'a, 'b: 'a>(
        &self,
        ast: &'b RwLockReadGuard<Ast<T>>,
        lhs: NodeID,
        rhs: NodeID,
    ) -> TypeInferenceResult<&'a NodeType> {
        let lhs_tp = self.get_type(ast, lhs)?;
        let rhs_tp = self.get_type(ast, rhs)?;
        if lhs_tp == rhs_tp {
            Ok(lhs_tp)
        } else {
            Err(Fail(ast::Err::single(
                "Left and right hand sides do not match",
                "",
                vec![lhs, rhs],
            )))
        }
    }

    fn get_type_node_void<'a, 'b: 'a>(
        &self,
        ast: &'b RwLockReadGuard<Ast<T>>,
        node_id: NodeID,
    ) -> TypeInferenceResult<&'a NodeType> {
        match self.get_type(ast, node_id)? {
            NodeType::Void => Err(Fail(ast::Err::single(
                "Did not expect void value here",
                "value is void",
                vec![node_id],
            ))),
            tp => Ok(tp),
        }
    }

    fn ast(&self) -> RwLockReadGuard<Ast<T>> {
        self.asts.get(self.ast_id)
    }

    fn ast_mut(&self) -> RwLockWriteGuard<Ast<T>> {
        self.asts.get_mut(self.ast_id)
    }

    fn get_type<'a, 'b: 'a>(
        &self,
        ast: &'b RwLockReadGuard<Ast<T>>,
        node_id: NodeID,
    ) -> TypeInferenceResult<&'a NodeType> {
        let node_id = node_id;
        if let Some(tp) = ast.get_node(node_id).maybe_tp() {
            Ok(tp)
        } else {
            Err(TypeInferenceErr::BlockedBy(node_id))
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

    fn infer_type(&self, node: &Node<T>) -> result::Result<InferredType, TypeInferenceErr> {
        use super::NodeType::*;
        use super::NodeTypeSource::*;
        use crate::ast::nodebody::NodeBody::*;
        use TypeInferenceErr::*;
        let ast = self.ast();
        match &node.body {
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
                    None => self.node_value_type(value.into()),
                };
                Ok(InferredType::new(tp, Declared))
            }
            Op { op, lhs, rhs } => {
                use ArithmeticOP::*;
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
                    None => Err(BlockedBy(*node_id)),
                }
            }
            VariableValue { variable, path } => {
                let mut value_tp = self.get_type(&ast, *variable)?;
                if let NodeType::NewType { .. } = value_tp {
                    let body = &ast.get_node(*variable).body;
                    match body {
                            NodeBody::ConstDeclaration{expr, ..}
                            | NodeBody::StaticDeclaration{expr, ..}=> {
                                value_tp = self.get_type(&ast, *expr)?;
                            }
                            NodeBody::TypeDeclaration{ ident, ..} => {
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
            TypeDeclaration { tp, .. } => match ast.get_node_type(*tp) {
                Some(tp) => Ok(InferredType::new(tp.clone(), Declared)),
                None => Err(BlockedBy(*tp)),
            },
            Import { expr, .. } => {
                let tp = self.get_type(&ast, *expr)?;
                Ok(InferredType::new(tp.clone(), Value))
            }
            VariableAssignment { .. } => Ok(InferredType::new(Void, Declared)),
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
            Empty
            | Break { .. }
            | Return { .. }
            | Block { .. }
            | If { .. }
            | Loop { .. }
            | Comment(..) => Ok(InferredType::new(Void, Declared)),
            Unlinked(_) => unreachable!(),
        }
    }

    fn infer_all_types(&mut self) -> TyperResult {
        use TypeInferenceErr::*;
        {
            let blocked = std::mem::replace(&mut self.blocked, HashMap::new());
            self.queue.extend(blocked.into_values().flatten());
        }

        let mut made_progress = false;
        let ast_id = self.ast_id;
        while let Some(node_id) = self.queue.pop_front() {
            let tp = {
                let ast = self.asts.get(self.ast_id);
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
                        node.infer_type(tp);
                    }
                    if let Some(blocked) = self.blocked.remove(&node_id) {
                        for blocked_id in blocked {
                            self.queue.push_back(blocked_id);
                        }
                    }
                }
                Err(BlockedBy(blocking_id)) => {
                    if !self.blocked.contains_key(&blocking_id) {
                        self.blocked.insert(blocking_id, vec![]);
                    }
                    self.blocked.get_mut(&blocking_id).unwrap().push(node_id);
                }
                Err(Fail(err)) => return TyperResult::Failed(err),
            };
        }

        if self.blocked.is_empty() {
            TyperResult::Complete
        } else {
            for (blocking_id, _) in self.blocked.iter() {
                if blocking_id.ast() != ast_id {
                    return TyperResult::BlockedBy {
                        blocked_by: blocking_id.ast(),
                        made_progress,
                    };
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

enum TyperResult {
    Complete,
    BlockedBy {
        blocked_by: AstID,
        made_progress: bool,
    },
    Failed(ast::Err),
}
