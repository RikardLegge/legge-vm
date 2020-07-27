use crate::ast::{Ast, Error, Node, NodeID, NodeType, NodeValue, Result};
use std::collections::VecDeque;

pub fn infer_types(ast: &mut Ast) -> Result<()> {
    Typer::new(ast).infer_types()
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

    fn try_coerce<'b, 'c: 'b>(
        &self,
        tp1_opt: &'b Option<NodeType>,
        tp2_opt: &'c Option<NodeType>,
    ) -> &'b Option<NodeType> {
        match (tp1_opt, tp2_opt) {
            (Some(tp1), Some(tp2)) => {
                if tp1 == tp2 {
                    return tp1_opt;
                }
                match (tp1, tp2) {
                    (NodeType::NotYetImplemented, _) => tp2_opt,
                    (_, NodeType::NotYetImplemented) => tp1_opt,
                    _ => &None,
                }
            }
            _ => &None,
        }
    }

    fn get_type(&self, node_id: &NodeID) -> &Option<NodeType> {
        let node = self.ast.get_node(*node_id);
        &node.tp
    }

    fn get_ref_type(&self, node_id: &NodeID) -> &Option<NodeType> {
        let mut tp = &None;
        let node = self.ast.get_node(*node_id);
        for ref_id in &node.referenced_by {
            let ref_tp = self.get_type(ref_id);
            if ref_tp.is_some() {
                tp = ref_tp;
                break;
            }
        }
        tp
    }

    fn get_type_from_string(&self, node_id: &NodeID, tp: &str) -> &Option<NodeType> {
        match tp {
            "int" => &Some(NodeType::Int),
            "void" => &Some(NodeType::Void),
            _ => &None,
        }
    }

    pub fn infer_types(mut self) -> Result<()> {
        use super::NodeBody::*;
        while let Some(node_id) = self.queue.pop_front() {
            let node = self.ast.get_node(node_id);
            for &child_id in node.body.children() {
                if let None = self.ast.get_node(child_id).tp {
                    self.queue.push_back(child_id)
                }
            }
            let tp = match &node.body {
                Value(value) => match value {
                    NodeValue::Int(..) => &Some(NodeType::Int),
                },
                Op(_, lhs, rhs) => {
                    let lhs = self.get_type(lhs);
                    let rhs = self.get_type(rhs);
                    self.try_coerce(lhs, rhs)
                }
                ProcedureDeclaration(args, returns, body) => &Some(NodeType::Fn),
                PrefixOp(_, node_id) => self.get_type(node_id),
                VariableValue(value) => self.get_type(value),
                Expression(_) => &Some(NodeType::NotYetImplemented),
                VariableDeclaration(_, declared, value) => {
                    if let Some(declared) = declared {
                        self.get_type_from_string(&node_id, declared)
                    } else if let Some(value) = value {
                        self.get_type(value)
                    } else {
                        self.get_ref_type(&node.id)
                    }
                }
                ConstDeclaration(_, declared, value) => {
                    if let Some(declared) = declared {
                        self.get_type_from_string(&node_id, declared)
                    } else {
                        let value_tp = self.get_type(value);
                        if let Some(_) = value_tp {
                            value_tp
                        } else {
                            self.get_ref_type(&node.id)
                        }
                    }
                }
                VariableAssignment(var, value) => {
                    let tp = self.get_type(var);
                    if let Some(_) = tp {
                        tp
                    } else {
                        self.get_type(value)
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
                            Some(return_type) => self.get_type_from_string(&proc.id, &return_type),
                            None => &Some(NodeType::Void),
                        },
                        _ => Err(Error::new(&format!(
                            "It's currently only possible to call functions, tried to call {:?}",
                            var
                        )))?,
                    }
                }
                Empty | Break(..) | Return(..) | Block(..) | If(..) | Loop(..) | Comment(..) => {
                    &Some(NodeType::Void)
                }
                Unlinked(_) => unreachable!(),
            };
            match tp {
                Some(_) => {
                    let tp = tp.clone();
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
                            "Failed to complete typecheck, unable to infer type of {:?}.\nOther nodes in the queue are: {:?}",
                            node, nodes
                        )));
                    }
                    self.since_last_changed += 1;
                    self.queue.push_back(node.id);
                }
            }
        }
        Ok(())
    }
}
