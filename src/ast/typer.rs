use crate::ast::ast::InferredType;
use crate::ast::{Ast, Error, Node, NodeID, NodeReferenceType, NodeType, NodeValue, Result};
use std::collections::VecDeque;

pub fn infer_types(ast: &mut Ast) -> Result<()> {
    Typer::new(ast).infer_all_types()
}

pub struct Typer<'a> {
    queue: VecDeque<NodeID>,
    ast: &'a mut Ast,
    since_last_changed: usize,
}

impl<'a> Typer<'a> {
    pub fn new(ast: &'a mut Ast) -> Self {
        let queue = VecDeque::from(vec![ast.root()]);
        let since_last_changed = 0;
        Self {
            queue,
            ast,
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

    fn get_type(&self, node_id: &NodeID) -> Option<NodeType> {
        match self.get_inferred_type(node_id) {
            Some(inf) => Some(inf.tp.clone()),
            None => None,
        }
    }

    fn get_inferred_type(&self, node_id: &NodeID) -> &Option<InferredType> {
        &self.ast.get_node(*node_id).tp
    }

    fn get_ref_type(&self, node_id: &NodeID, ref_tp: NodeReferenceType) -> Option<NodeType> {
        let mut tp = None;
        let node = self.ast.get_node(*node_id);
        for node_ref in &node.referenced_by {
            if node_ref.ref_tp == ref_tp {
                let ref_tp = self.get_type(&node_ref.id);
                if ref_tp.is_some() {
                    tp = ref_tp;
                    break;
                }
            }
        }
        tp
    }

    fn get_type_from_string(&self, node_id: &NodeID, tp: &str) -> Option<NodeType> {
        match tp {
            "int" => Some(NodeType::Int),
            "void" => Some(NodeType::Void),
            _ => None,
        }
    }

    pub fn infer_type(&self, node: &Node) -> Result<Option<InferredType>> {
        use super::NodeBody::*;
        use super::NodeReferenceType::*;
        use super::NodeType::*;
        use super::NodeTypeSource::*;
        let tp = match &node.body {
            RuntimeReference(..) => unimplemented!(),
            ConstValue(value) => match value {
                NodeValue::Int(..) => Some(InferredType::new(Int, Declared)),
                NodeValue::String(..) => Some(InferredType::new(String, Declared)),
            },
            Op(_, lhs, rhs) => {
                let lhs = self.get_type(lhs);
                let rhs = self.get_type(rhs);
                let tp = self.try_coerce(lhs, rhs);
                InferredType::maybe(tp, Value)
            }
            ProcedureDeclaration(args, returns, body) => {
                let arg_types: Vec<Option<NodeType>> =
                    args.iter().map(|id| self.get_type(id)).collect();
                let args_inferred = arg_types.iter().all(|tp| tp.is_some());
                if args_inferred {
                    let arg_types = arg_types.into_iter().map(|tp| tp.unwrap()).collect();
                    let return_type = match returns {
                        Some(name) => self.get_type_from_string(&node.id, name),
                        None => Some(Void),
                    };
                    if let Some(return_type) = return_type {
                        InferredType::maybe(Some(Fn(arg_types, Box::new(return_type))), Declared)
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            PrefixOp(_, node_id) => InferredType::maybe(self.get_type(node_id), Value),
            VariableValue(value) => InferredType::maybe(self.get_type(value), Value),
            Expression(_) => Some(InferredType::new(NotYetImplemented, Declared)),
            VariableDeclaration(_, declared, value) => {
                if let Some(declared) = declared {
                    InferredType::maybe(self.get_type_from_string(&node.id, declared), Declared)
                } else if let Some(value) = value {
                    InferredType::maybe(self.get_type(value), Value)
                } else {
                    InferredType::maybe(self.get_ref_type(&node.id, ReceiveValue), Usage)
                }
            }
            ConstDeclaration(_, declared, value) => {
                if let Some(declared) = declared {
                    InferredType::maybe(self.get_type_from_string(&node.id, declared), Declared)
                } else if let Some(tp) = self.get_type(value) {
                    InferredType::maybe(Some(tp), Value)
                } else {
                    InferredType::maybe(self.get_ref_type(&node.id, ReceiveValue), Usage)
                }
            }
            VariableAssignment(var, value) => {
                if let Some(tp) = self.get_type(var) {
                    InferredType::maybe(Some(tp), Variable)
                } else {
                    InferredType::maybe(self.get_type(value), Value)
                }
            }
            Call(var_id, _) => {
                let var = self.ast.get_node(*var_id);
                let proc = match &var.body {
                    ConstDeclaration(.., proc_id) | VariableDeclaration(.., Some(proc_id)) => {
                        self.ast.get_node(*proc_id)
                    }
                    _ => Err(Error::new(&format!(
                        "Call must be referencing a variable declaration, {:?} found",
                        var
                    )))?,
                };
                match &proc.body {
                    ProcedureDeclaration(_, return_type, _) => match return_type {
                        Some(return_type) => InferredType::maybe(
                            self.get_type_from_string(&proc.id, &return_type),
                            Value,
                        ),
                        None => InferredType::maybe(Some(Void), Value),
                    },
                    RuntimeReference(ident) => match &proc.tp {
                        Some(tp) => Some(tp.clone()),
                        None => None,
                    },
                    _ => Err(Error::new(&format!(
                        "It's currently only possible to call functions, tried to call {:?}",
                        var
                    )))?,
                }
            }
            Empty | Break(..) | Return(..) | Block(..) | If(..) | Loop(..) | Comment(..) => {
                InferredType::maybe(Some(Void), Declared)
            }
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
            match tp {
                Some(_) => {
                    let node = self.ast.get_node_mut(node_id);
                    node.tp = tp;
                    self.since_last_changed = 0;
                }
                None => {
                    if self.since_last_changed > self.queue.len() {
                        let nodes: Vec<&Node> =
                            self.queue.iter().map(|id| self.ast.get_node(*id)).collect();
                        // return Ok(());
                        return Err(Error::new(&format!(
                            "Failed to complete type check, unable to infer type of {:?}.\nOther nodes in the queue are: {:?}",
                            self.ast.get_node(node_id), nodes
                        )));
                    }
                    self.since_last_changed += 1;
                    self.queue.push_back(node_id);
                }
            }
        }
        Ok(())
    }
}
