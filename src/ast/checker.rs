use super::{Ast, Node, Result};
use crate::ast::{NodeBody, NodeID, NodeType};

pub fn check_types(ast: &Ast) -> Result<()> {
    Checker::new(ast).check_all_types(ast.root())
}

pub struct Checker<'a> {
    ast: &'a Ast,
}

impl<'a> Checker<'a> {
    pub fn new(ast: &'a Ast) -> Self {
        Self { ast }
    }

    pub fn check_type(&self, node: &Node) -> Result<()> {
        use NodeBody::*;
        use NodeType::*;
        if node.tp.is_some() {
            match &node.body {
                Empty
                | Break { .. }
                | Comment { .. }
                | ConstValue { .. }
                | PrefixOp { .. }
                | Block { .. }
                | Loop { .. }
                | Expression { .. }
                | VariableValue { .. }
                | ProcedureDeclaration { .. }
                | Import { .. } => Ok(()),
                Op { op, lhs, rhs } => {
                    let lhs_tp = self.ast.get_node(*lhs).tp.as_ref().unwrap();
                    let rhs_tp = self.ast.get_node(*rhs).tp.as_ref().unwrap();
                    if lhs_tp.tp == rhs_tp.tp {
                        Ok(())
                    } else {
                        Err(self.ast.error(
                            &format!(
                                "Types for left and right hand side of op {:?} do not match ({:?} != {:?})",
                                op, lhs_tp.tp, rhs_tp.tp
                            ),
                            "Both sides must have the same type",
                            vec![node.id, *lhs, *rhs],
                        ))
                    }
                }
                VariableAssignment {
                    variable,
                    path,
                    expr,
                } => {
                    let variable_node = self.ast.get_node(*variable);
                    match variable_node.body {
                        NodeBody::ConstDeclaration { .. } | NodeBody::StaticDeclaration { .. } => {
                            Err(self.ast.error(
                                "Not allowed to assign to constant value",
                                "Assignment to constant value",
                                vec![node.id],
                            ))?
                        }
                        _ => (),
                    }
                    let mut variable_tp = &variable_node.tp.as_ref().unwrap().tp;
                    if let Some(path) = path {
                        for path_field in path {
                            if let NodeType::Struct { fields } = &variable_tp {
                                let mut tp = None;
                                for (field, field_tp) in fields {
                                    if path_field == field {
                                        tp = Some(field_tp);
                                        break;
                                    }
                                }
                                match tp {
                                    Some(tp) => {
                                        variable_tp = tp;
                                    }
                                    None => {
                                        return Err(self.ast.error(
                                            &format!(
                                                "Struct does not have the field '{}'",
                                                path_field
                                            ),
                                            "",
                                            vec![node.id],
                                        ));
                                    }
                                }
                            } else {
                                unimplemented!()
                            }
                        }
                    }
                    let expr_tp = &self.ast.get_node(*expr).tp.as_ref().unwrap().tp;
                    if variable_tp == expr_tp {
                        Ok(())
                    } else {
                        Err(self.ast.error(
                            &format!("Types for left and right hand side of assignment do not match ({:?} != {:?})", variable_tp, expr_tp
                            ),
                            "Both sides of an assignment must have the same type",
                            vec![self.ast.get_node(*expr).parent_id.unwrap(), *expr],
                        ))
                    }
                }
                If { condition, .. } => {
                    let statement_tp = self.ast.get_node(*condition).tp.as_ref().unwrap();
                    if statement_tp.tp == Bool {
                        Ok(())
                    } else {
                        Err(self.ast.error(
                            &format!("The condition of an if statement must be of type Bool, ({:?}) was found", statement_tp.tp),
                            "Both sides of an assignment must have the same type",
                            vec![*condition],
                        ))
                    }
                }
                VariableDeclaration { expr, .. } => {
                    if let Some(expr) = expr {
                        let lhs = node.tp.as_ref().unwrap();
                        let rhs = self.ast.get_node(*expr).tp.as_ref().unwrap();
                        if lhs.tp == rhs.tp {
                            Ok(())
                        } else {
                            unreachable!()
                        }
                    } else {
                        Ok(())
                    }
                }
                TypeDeclaration { constructor, .. } => {
                    let lhs = node.tp.as_ref().unwrap();
                    let rhs = self.ast.get_node(*constructor).tp.as_ref().unwrap();
                    if let Type { tp } = &lhs.tp {
                        if let Fn { returns, .. } = &rhs.tp {
                            if tp == returns {
                                return Ok(());
                            } else {
                                println!("{:?} != {:?}", lhs.tp, rhs.tp);
                                panic!();
                            }
                        }
                    }
                    unreachable!()
                }

                ConstDeclaration { expr, .. } | StaticDeclaration { expr, .. } => {
                    let lhs = node.tp.as_ref().unwrap();
                    let rhs = self.ast.get_node(*expr).tp.as_ref().unwrap();
                    if lhs.tp == rhs.tp {
                        Ok(())
                    } else {
                        Err(self.ast.error(
                            &format!(
                                "Left and right hand side must have the same types, '{:?}' != '{:?}'",
                                lhs.tp, rhs.tp
                            ),
                            "",
                            vec![*expr, self.ast.get_node(*expr).parent_id.unwrap()],
                        ))
                    }
                }
                Return { func, expr } => {
                    let func = self.ast.get_node(*func).tp.as_ref().unwrap();
                    if let Fn { returns, .. } = &func.tp {
                        match (&**returns, expr) {
                            (NodeType::Void, None) => Ok(()),
                            (NodeType::Void, Some(ret_id)) => Err(self.ast.error(
                                "Return value should be of type void.",
                                "Not allowed to return a value from here",
                                vec![*ret_id],
                            )),
                            (_, None) => Err(self.ast.error(
                                "Return value can not be of type void",
                                "A value must be provided when returning from here",
                                vec![node.id],
                            )),
                            (tp, Some(ret_id)) => {
                                let ret = self.ast.get_node(*ret_id).tp.as_ref().unwrap();
                                if ret.tp == *tp {
                                    Ok(())
                                } else {
                                    Err(self.ast.error(
                                        &format!("Return statement does not return the right type, {:?} expected, {:?} provided", func.tp, ret.tp),
                                        "Wrong return type for function",
                                        vec![*ret_id],
                                    ))
                                }
                            }
                        }
                    } else {
                        unreachable!()
                    }
                }
                Call { func, args } => {
                    let func = &self.ast.get_node(*func).tp.as_ref().unwrap().tp;
                    let args = args
                        .iter()
                        .map(|id| self.ast.get_node(*id).tp.as_ref().unwrap().tp.clone())
                        .collect();
                    let call = &match func {
                        Fn { returns, .. } | Type { tp: returns } => Fn {
                            args,
                            returns: returns.clone(),
                        },
                        _ => unreachable!(),
                    };
                    self.fits_function_call(node, func, call)?;
                    Ok(())
                }
                Unlinked { .. } => Err(self.ast.error(
                    "Encountered a node with unlinked type",
                    "expression with unlinked type",
                    vec![node.id],
                )),
            }
        } else {
            Err(self.ast.error(
                "Encountered a node without a type",
                "expression with missing type",
                vec![node.id],
            ))
        }
    }

    fn correct_number_of_arguments<'b>(
        &'_ self,
        node: &'b Node,
        hole_args: &'b [NodeType],
        shape_args: &'b [NodeType],
    ) -> Result<Option<&'b NodeType>> {
        let invalid_varargs = hole_args.iter().rev().skip(1).position(|a| {
            if let NodeType::VarArg { .. } = a {
                true
            } else {
                false
            }
        });
        let hole_args_len = if let Some(NodeType::VarArg { .. }) = hole_args.last() {
            hole_args.len() - 1
        } else {
            hole_args.len()
        };
        if let Some(i) = invalid_varargs {
            let invalid_node = node.body.children().skip(i).next();
            if let Some(invalid_node) = invalid_node {
                Err(self.ast.error(
                    &format!("miss placed vararg",),
                    "Invalid vararg",
                    vec![*invalid_node],
                ))
            } else {
                Err(self.ast.error(
                    &format!("missplaced vararg",),
                    "Too many arguments",
                    vec![node.id],
                ))
            }
        } else if hole_args_len < shape_args.len() {
            if let Some(NodeType::VarArg { args }) = hole_args.last() {
                Ok(Some(&*args))
            } else {
                let skip = hole_args.len();
                let nodes = node.body.children().skip(skip).map(|id| *id).collect();
                Err(self.ast.error(
                    &format!(
                        "wrong number of arguments provided to function, {} expected, {} provided",
                        hole_args.len(),
                        shape_args.len()
                    ),
                    "Too many arguments",
                    nodes,
                ))
            }
        } else if hole_args_len > shape_args.len() {
            let nodes = node.body.children().map(|id| *id).collect();
            Err(self.ast.error(
                &format!(
                    "wrong number of arguments provided to function, {} expected, {} provided",
                    hole_args.len(),
                    shape_args.len()
                ),
                "Too few arguments",
                nodes,
            ))
        } else {
            Ok(None)
        }
    }

    fn fits_function_argument(&self, node: &Node, hole: &NodeType, shape: &NodeType) -> Result<()> {
        use NodeType::*;
        match (hole, shape) {
            (
                Fn {
                    args: hole_args,
                    returns: hole_ret,
                },
                Fn {
                    args: shape_args,
                    returns: shape_ret,
                },
            ) => {
                self.fits_function_argument(node, hole_ret, shape_ret)?;
                let vararg_tp = self.correct_number_of_arguments(node, &hole_args, &shape_args)?;
                let arg_node_ids = node.body.children().map(|id| *id).collect::<Vec<NodeID>>();
                if let Some(_) = vararg_tp {
                    unimplemented!();
                } else {
                    for (i, (shape_arg, hole_arg)) in shape_args.iter().zip(hole_args).enumerate() {
                        let arg = if i >= arg_node_ids.len() {
                            node
                        } else {
                            self.ast.get_node(arg_node_ids[i])
                        };
                        // Intentionally flipped shape and hole since we are comparing type declarations
                        self.fits_function_argument(arg, shape_arg, hole_arg)?;
                    }
                }
                Ok(())
            }
            (Any, Void) if *shape != Void => Err(self.ast.error(
                "function expected Any type of value, nothing (Void) provided.",
                "Argument required here",
                vec![node.id],
            )),
            (Any, _) => Ok(()),
            (hole, shape) if hole == shape => Ok(()),
            _ => Err(self.ast.error(
                &format!(
                    "function arguments are of the wrong type, {:?} expected, {:?} provided",
                    hole, shape
                ),
                "Wrong argument type",
                vec![node.id],
            )),
        }
    }

    fn fits_function_call(&self, node: &Node, hole: &NodeType, shape: &NodeType) -> Result<()> {
        use NodeType::*;
        match (hole, shape) {
            (Type { tp: fields }, Fn { args, returns }) => {
                if args.len() > 0 {
                    let nodes = node.body.children().map(|id| *id).collect();
                    Err(self.ast.error(
                        "Type constructor can not have arguments",
                        "Arguments not allowed here",
                        nodes,
                    ))
                } else if fields != returns {
                    Err(self.ast.error(
                        "constructor does not return correct struct",
                        "",
                        vec![node.id],
                    ))
                } else {
                    Ok(())
                }
            }
            (
                Fn {
                    args: hole_args,
                    returns: hole_ret,
                },
                Fn {
                    args: shape_args,
                    returns: shape_ret,
                },
            ) => {
                self.fits_function_argument(node, hole_ret, shape_ret)?;
                let vararg_tp = self.correct_number_of_arguments(node, &hole_args, &shape_args)?;
                let arg_node_ids = node.body.children().map(|id| *id).collect::<Vec<NodeID>>();
                if let Some(vararg) = vararg_tp {
                    for (i, shape_arg) in shape_args.iter().enumerate() {
                        let hole_arg = if i < hole_args.len() - 1 {
                            &hole_args[i]
                        } else {
                            vararg
                        };
                        let arg = self.ast.get_node(arg_node_ids[i]);
                        self.fits_function_argument(arg, hole_arg, shape_arg)?;
                    }
                } else {
                    for (i, (shape_arg, hole_arg)) in shape_args.iter().zip(hole_args).enumerate() {
                        let arg = self.ast.get_node(arg_node_ids[i]);
                        self.fits_function_argument(arg, hole_arg, shape_arg)?;
                    }
                }
                Ok(())
            }
            (_, Fn { .. }) => Err(self.ast.error(
                &format!("tried to call non-callable function"),
                "Can not call",
                vec![node.id],
            )),
            (hole, shape) => unreachable!("{:?} =! {:?}", hole, shape),
        }
    }

    pub fn check_all_types(&self, root_id: NodeID) -> Result<()> {
        let root = self.ast.get_node(root_id);
        self.check_type(root)?;
        for child_id in root.body.children() {
            self.check_all_types(*child_id)?;
        }
        Ok(())
    }
}
