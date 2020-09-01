use crate::ast::ast::InferredType;
use crate::ast::ast::NodeTypeSource::Usage;
use crate::ast::{Ast, Node, NodeID, NodeType, NodeValue, Result};
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
            None => None,
        }
    }

    fn get_inferred_type(&self, node_id: &NodeID) -> &Option<InferredType> {
        &self.ast.get_node(*node_id).tp
    }

    // fn get_ref_type(&self, node_id: &NodeID, ref_tp: NodeReferenceType) -> Option<NodeType> {
    //     let mut tp = None;
    //     let node = self.ast.get_node(*node_id);
    //     for node_ref in &node.referenced_by {
    //         if node_ref.ref_tp == ref_tp {
    //             let ref_tp = self.get_type(&node_ref.id);
    //             if ref_tp.is_some() {
    //                 tp = ref_tp;
    //                 break;
    //             }
    //         }
    //     }
    //     tp
    // }

    fn get_type_from_declaration(&self, _: &NodeID, tp: &NodeType) -> Result<Option<NodeType>> {
        Ok(Some(tp.clone()))
    }

    pub fn infer_type(&self, node: &Node) -> Result<Option<InferredType>> {
        use super::NodeBody::*;
        use super::NodeType::*;
        use super::NodeTypeSource::*;
        let tp = match &node.body {
            ConstValue(value) => match value {
                NodeValue::Int(..) => Some(InferredType::new(Int, Declared)),
                NodeValue::String(..) => Some(InferredType::new(String, Declared)),
                NodeValue::Bool(..) => Some(InferredType::new(Bool, Declared)),
                NodeValue::RuntimeFn(id) => {
                    let func = &self.runtime.functions[*id];
                    let tp = func.tp.clone();
                    Some(InferredType::new(tp, Declared))
                }
            },
            Op(op, lhs, rhs) => {
                use ArithmeticOP::*;
                match op {
                    Eq => InferredType::maybe(Some(Bool), Declared),
                    Add | Sub | Mul | Div => {
                        let lhs = self.get_type(lhs);
                        let rhs = self.get_type(rhs);
                        let tp = self.try_coerce(lhs, rhs);
                        InferredType::maybe(tp, Value)
                    }
                }
            }
            ProcedureDeclaration(args, returns, _) => {
                let arg_types: Vec<Option<NodeType>> =
                    args.iter().map(|id| self.get_type(id)).collect();
                let args_inferred = arg_types.iter().all(|tp| tp.is_some());
                if args_inferred {
                    let arg_types = arg_types.into_iter().map(|tp| tp.unwrap()).collect();
                    let return_type = match returns {
                        Some(tp) => self.get_type_from_declaration(&node.id, tp)?,
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
                    InferredType::maybe(
                        self.get_type_from_declaration(&node.id, declared)?,
                        Declared,
                    )
                } else if let Some(value) = value {
                    self.not_void(value)?;
                    InferredType::maybe(self.get_type(value), Value)
                } else {
                    // InferredType::maybe(self.get_ref_type(&node.id, ReceiveValue), Usage)
                    None
                }
            }
            ConstDeclaration(_, declared, value) => {
                if let Some(declared) = declared {
                    InferredType::maybe(
                        self.get_type_from_declaration(&node.id, declared)?,
                        Declared,
                    )
                } else if let Some(tp) = self.get_type(value) {
                    self.not_void(value)?;
                    InferredType::maybe(Some(tp), Value)
                } else {
                    // InferredType::maybe(self.get_ref_type(&node.id, ReceiveValue), Usage)
                    None
                }
            }
            Import(_, value) => {
                if let Some(tp) = self.get_type(value) {
                    InferredType::maybe(Some(tp), Value)
                } else {
                    None
                }
            }
            VariableAssignment(var, value) => {
                if let Some(tp) = self.get_type(var) {
                    InferredType::maybe(Some(tp), Variable)
                } else {
                    self.not_void(value)?;
                    InferredType::maybe(self.get_type(value), Value)
                }
            }
            Call(var_id, _) => {
                let var = self.ast.get_node(*var_id);
                if let Some(tp) = &var.tp {
                    match &tp.tp {
                        Fn(_, ret) => {
                            InferredType::maybe(Some((**ret).clone()), Value)
                        }
                        _ => Err(self.ast.error(
                            &format!(
                                "It's currently only possible to call functions, tried to call {:?}",
                                var
                            ),
                            "",
                            vec![node.id, *var_id],
                        ))?
                    }
                } else {
                    None
                }
                // match var.body {
                //     ConstDeclaration(.., proc_id)
                //     | VariableDeclaration(.., Some(proc_id))
                //     | Import(.., proc_id) => {
                //         let proc = self.ast.get_node(*proc_id);
                //         match &proc.body {
                //             ProcedureDeclaration(_, return_type, _) => match return_type {
                //                 Some(return_type) => InferredType::maybe(
                //                     self.get_type_from_declaration(&proc.id, &return_type)?,
                //                     Value,
                //                 ),
                //                 None => InferredType::maybe(Some(Void), Value),
                //             },
                //             ConstValue(NodeValue::RuntimeFn(id)) => {
                //                 let tp = self.runtime.functions[*id].returns.clone();
                //                 InferredType::maybe(Some(tp), Value)
                //             }
                //             _ => Err(self.ast.error(
                //                 &format!(
                //                     "It's currently only possible to call functions, tried to call {:?}",
                //                     var
                //                 ),
                //                 "",
                //                 vec![node.id, *var_id],
                //             ))?,
                //         }
                //     }
                //     VariableDeclaration(..) => None,
                //     _ => Err(self.ast.error(
                //         &format!(
                //             "Call must be referencing a variable declaration, {:?} found",
                //             var
                //         ),
                //         "",
                //         vec![node.id],
                //     ))?,
                // };
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
            if let Some(tp) = tp {
                let source = tp.source;
                let node = self.ast.get_node_mut(node_id);
                node.tp = Some(tp);
                // Do not mark values which are only used as type checked
                if source != Usage {
                    self.since_last_changed = 0;
                    continue;
                }
            }

            self.since_last_changed += 1;
            self.queue.push_back(node_id);
            if self.since_last_changed > self.queue.len() + 1 {
                return Err(self.ast.error(
                    "Failed to complete type check, unable to infer the type of the following expressions",
                    "unable to infer type"
                    , self.queue.into()));
            }
        }
        Ok(())
    }
}
