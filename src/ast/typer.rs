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
            None => None,
        }
    }

    fn get_inferred_type(&self, node_id: &NodeID) -> &Option<InferredType> {
        let node = self.ast.get_node(*node_id);
        &node.tp
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
                NodeType::Struct(field_types)
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
            ConstValue(value) => Some(InferredType::new(self.node_value_type(value), Declared)),
            Op(op, lhs, rhs) => {
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
            VariableValue(value, path) => {
                if let Some(value_tp) = self.get_type(value) {
                    let mut value_tp = value_tp;
                    if let NodeType::Type(_) = value_tp {
                        let body = &self.ast.get_node(*value).body;
                        match body {
                            NodeBody::ConstDeclaration(_, _, constructor)
                            | NodeBody::StaticDeclaration(_, _, constructor)=> {
                                value_tp = self.get_type(constructor).unwrap();
                            }
                            NodeBody::TypeDeclaration(name, ..) => {
                                Err(self.ast.error(
                                        "When instantiating a type, use the default instantiation function by adding ()",
                                    &format!("Replace with {}()", name),
                                    vec![node.id],
                                ))?
                            }
                            _ => unreachable!(),
                        }
                    }
                    let mut value_tp = &value_tp;
                    if let Some(path) = path {
                        for path_field in path {
                            if let NodeType::Struct(fields) = &value_tp {
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
                            } else {
                                self.ast.unimplemented(node.id)?;
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
                    None => None
                }
            },
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
                    None
                }
            }
            ConstDeclaration(_, declared, value)
            | StaticDeclaration(_, declared, value) => {
                if let Some(declared) = declared {
                    InferredType::maybe(
                        self.get_type_from_declaration(&node.id, declared)?,
                        Declared,
                    )
                } else if let Some(tp) = self.get_type(value) {
                    self.not_void(value)?;
                    InferredType::maybe(Some(tp), Value)
                } else {
                    None
                }
            }
            TypeDeclaration(_, declared, ..) => InferredType::maybe(
                self.get_type_from_declaration(&node.id, declared)?,
                Declared,
            ),
            Import(_, value) => {
                if let Some(tp) = self.get_type(value) {
                    InferredType::maybe(Some(tp), Value)
                } else {
                    None
                }
            }
            VariableAssignment(..) => InferredType::maybe(Some(Void), Declared),
            Call(var_id, _) => {
                let var = self.ast.get_node(*var_id);
                if let Some(tp) = &var.tp {
                    match &tp.tp {
                        Fn(_, ret) => {
                            InferredType::maybe(Some((**ret).clone()), Value)
                        }
                        Type(ret) => {
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
                if source != NodeTypeSource::Usage {
                    self.since_last_changed = 0;
                    continue;
                }
            }

            self.since_last_changed += 1;
            self.queue.push_back(node_id);
            if self.since_last_changed > 2 * self.queue.len() {
                println!("{:?}", self.ast);
                return Err(self.ast.error(
                    "Failed to complete type check, unable to infer the type of the following expressions",
                    "unable to infer type"
                    , self.queue.into()));
            }
        }
        Ok(())
    }
}
