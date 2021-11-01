use crate::ast::ast::InferredType;
use crate::ast::{Ast, Node, NodeBody, NodeID, NodeType, NodeTypeSource, NodeValue, Result};
use crate::runtime::Runtime;
use crate::token::ArithmeticOP;
use std::collections::VecDeque;

pub fn infer_types(ast: &mut Ast, runtime: &Runtime) -> Result<()> {
    Typer::new(ast, runtime).infer_all_types()
}

pub struct Typer<'a> {
    queue: VecDeque<NodeID>,
    ast: &'a mut Ast,
    runtime: &'a Runtime,
    since_last_changed: usize,
}

impl<'a> Typer<'a> {
    pub fn new(ast: &'a mut Ast, runtime: &'a Runtime) -> Self {
        let queue = VecDeque::from(vec![ast.root()]);
        let since_last_changed = 0;
        Self {
            queue,
            ast,
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
            Some(NodeType::Void) => Err(self.ast.error(
                "Did not expect void value here",
                "value is void",
                vec![*node_id],
            )),
            _ => Ok(()),
        }
    }

    fn get_type(&self, node_id: &NodeID) -> Option<NodeType> {
        match self.get_inferred_type(node_id) {
            Some(inf) => Some(inf.tp.clone()),
            None => self.ast.partial_type(*node_id).map(|(_, tp)| tp.clone()),
        }
    }

    fn get_inferred_type(&self, node_id: &NodeID) -> &Option<InferredType> {
        let node = self.ast.get_node(*node_id);
        &node.tp
    }

    fn node_value_type(&self, value: &NodeValue) -> NodeType {
        match value {
            NodeValue::Int(..) => NodeType::Int,
            NodeValue::Float(..) => NodeType::Float,
            NodeValue::String(..) => NodeType::String,
            NodeValue::Bool(..) => NodeType::Bool,
            NodeValue::Struct(fields) => {
                let field_types = fields
                    .iter()
                    .map(|(name, value)| {
                        let tp = self.node_value_type(value);
                        (name.clone(), tp)
                    })
                    .collect();
                NodeType::Struct {
                    fields: field_types,
                }
            }
            NodeValue::RuntimeFn(id) => {
                let func = &self.runtime.functions[*id];
                func.tp.clone()
            }
            NodeValue::Unlinked(_) => unreachable!(),
        }
    }

    pub fn infer_type(&self, node: &Node) -> Result<Option<InferredType>> {
        use super::NodeBody::*;
        use super::NodeType::*;
        use super::NodeTypeSource::*;
        let tp = match &node.body {
            TypeReference { tp } => self
                .ast
                .partial_type(*tp)
                .map(|(_, tp)| InferredType::new(tp.clone(), NodeTypeSource::Declared)),
            PartialType { tp, .. } => tp
                .option()
                .map(|tp| InferredType::new(tp.clone(), NodeTypeSource::Declared)),
            ConstValue { value, tp } => {
                let tp = match tp {
                    Some(tp_id) => match self.ast.partial_type(*tp_id) {
                        Some((_, tp)) => Some(tp.clone()),
                        None => None,
                    },
                    None => None,
                };
                let tp = match tp {
                    Some(tp) => tp,
                    None => self.node_value_type(value),
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
            ProcedureDeclaration { args, returns, .. } => {
                let arg_types: Vec<Option<NodeType>> =
                    args.iter().map(|id| self.get_type(id)).collect();
                let args_inferred = arg_types.iter().all(|tp| tp.is_some());
                if args_inferred {
                    let arg_types = arg_types.into_iter().map(|tp| tp.unwrap()).collect();
                    let return_type = match *returns {
                        Some(returns) => self.ast.get_node(returns).tp.clone().map(|t| t.tp),
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
            VariableValue { variable, path } => {
                if let Some(value_tp) = self.get_type(variable) {
                    let mut value_tp = value_tp;
                    if let NodeType::NewType { .. } = value_tp {
                        let body = &self.ast.get_node(*variable).body;
                        match body {
                            NodeBody::ConstDeclaration{expr, ..}
                            | NodeBody::StaticDeclaration{expr, ..}=> {
                                value_tp = self.get_type(expr).unwrap();
                            }
                            NodeBody::TypeDeclaration{ ident, ..} => {
                                Err(self.ast.error(
                                    "When instantiating a type, use the default instantiation function by adding ()",
                                    &format!("Replace with {}()", ident),
                                    vec![node.id],
                                ))?
                            }
                            _ => unreachable!("{:?}, {:?}", *variable, body),
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
                let tp = self.infer_type(self.ast.get_node(*value))?;
                match tp {
                    Some(tp) => Some(InferredType::new(tp.tp, Value)),
                    None => None,
                }
            }
            VariableDeclaration { tp, expr, .. } => {
                if let Some(tp) = tp {
                    InferredType::maybe(self.ast.get_node_type(*tp), Declared)
                } else if let Some(expr) = expr {
                    self.not_void(expr)?;
                    InferredType::maybe(self.get_type(expr), Value)
                } else {
                    None
                }
            }
            ConstDeclaration { tp, expr, .. } | StaticDeclaration { tp, expr, .. } => {
                if let Some(tp) = tp {
                    InferredType::maybe(self.ast.get_node_type(*tp), Declared)
                } else if let Some(expr_tp) = self.get_type(expr) {
                    self.not_void(expr)?;
                    InferredType::maybe(Some(expr_tp), Value)
                } else {
                    None
                }
            }
            TypeDeclaration { tp, .. } => {
                InferredType::maybe(self.ast.get_node_type(*tp), Declared)
            }
            Import { expr, .. } => {
                if let Some(tp) = self.get_type(expr) {
                    InferredType::maybe(Some(tp), Value)
                } else {
                    None
                }
            }
            VariableAssignment { .. } => InferredType::maybe(Some(Void), Declared),
            Call { func, .. } => {
                let var = self.ast.get_node(*func);
                if let Some(tp) = &var.tp {
                    match &tp.tp {
                        Fn{returns, ..} |
                        NewType {tp: returns, .. }=> {
                            InferredType::maybe(Some((**returns).clone()), Value)
                        }
                        _ => Err(self.ast.error(
                            &format!(
                                "It's currently only possible to call functions, tried to call {:?}",
                                var
                            ),
                            "",
                            vec![node.id, *func],
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

    pub fn infer_all_types(mut self) -> Result<()> {
        while let Some(node_id) = self.queue.pop_front() {
            let node = self.ast.get_node(node_id);
            for &child_id in node.body.children() {
                if let None = self.ast.get_node(child_id).tp {
                    self.queue.push_back(child_id)
                }
            }
            let tp = self.infer_type(node)?;
            if let Some(tp) = tp {
                let source = tp.source;
                let node = self.ast.get_node_mut(node_id);
                node.tp = Some(tp);
                // Do not mark values which are only used as type checked
                if source != NodeTypeSource::Usage {
                    self.since_last_changed = 0;
                    continue;
                }
            }

            self.since_last_changed += 1;
            self.queue.push_back(node_id);
            if self.since_last_changed > 2 * self.queue.len() {
                return Err(self.ast.error(
                    "Failed to complete type check, unable to infer the type of the following expressions",
                    "unable to infer type"
                    , self.queue.into()));
            }
        }
        Ok(())
    }
}
