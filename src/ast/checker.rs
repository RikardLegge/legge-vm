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
                | Break(..)
                | Comment(..)
                | ConstValue(..)
                | PrefixOp(..)
                | Block(..)
                | Loop(..)
                | Expression(..)
                | VariableValue(..)
                | ProcedureDeclaration(..)
                | Import(..) => Ok(()),
                Op(op, lhs, rhs) => {
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
                VariableAssignment(lhs, path, rhs) => {
                    let lhs_node = self.ast.get_node(*lhs);
                    match lhs_node.body {
                        NodeBody::ConstDeclaration(..) => Err(self.ast.error(
                            "Not allowed to assign to constant value",
                            "Assignment to constant value",
                            vec![node.id],
                        ))?,
                        _ => (),
                    }
                    let mut lhs_tp = &lhs_node.tp.as_ref().unwrap().tp;
                    if let Some(path) = path {
                        for path_field in path {
                            if let NodeType::Struct(fields) = &lhs_tp {
                                let mut tp = None;
                                for (field, field_tp) in fields {
                                    if path_field == field {
                                        tp = Some(field_tp);
                                        break;
                                    }
                                }
                                match tp {
                                    Some(tp) => {
                                        lhs_tp = tp;
                                    }
                                    None => {
                                        return Err(self.ast.error(
                                            &format!(
                                                "Struct does not have the field '{}'",
                                                path_field
                                            ),
                                            "",
                                            vec![node.id],
                                        ))
                                    }
                                }
                            } else {
                                unimplemented!()
                            }
                        }
                    }
                    let rhs_tp = &self.ast.get_node(*rhs).tp.as_ref().unwrap().tp;
                    if lhs_tp == rhs_tp {
                        Ok(())
                    } else {
                        Err(self.ast.error(
                            &format!("Types for left and right hand side of assignment do not match ({:?} != {:?})",                                 lhs_tp, rhs_tp
                            ),
                            "Both sides of an assignment must have the same type",
                            vec![node.id],
                        ))
                    }
                }
                If(statement, _) => {
                    let statement_tp = self.ast.get_node(*statement).tp.as_ref().unwrap();
                    if statement_tp.tp == Bool {
                        Ok(())
                    } else {
                        Err(self.ast.error(
                            &format!("The condition of an if statement must be of type Bool, ({:?}) was found", statement_tp.tp),
                            "Both sides of an assignment must have the same type",
                            vec![*statement],
                        ))
                    }
                }
                VariableDeclaration(_, _, value) => {
                    if let Some(value) = value {
                        let lhs = node.tp.as_ref().unwrap();
                        let rhs = self.ast.get_node(*value).tp.as_ref().unwrap();
                        if lhs.tp == rhs.tp {
                            Ok(())
                        } else {
                            unreachable!()
                        }
                    } else {
                        Ok(())
                    }
                }
                ConstDeclaration(_, _, value) => {
                    let lhs = node.tp.as_ref().unwrap();
                    let rhs = self.ast.get_node(*value).tp.as_ref().unwrap();
                    if let Type(fields_tp) = &lhs.tp {
                        if let Fn(_, ret_tp) = &rhs.tp {
                            if fields_tp == ret_tp {
                                return Ok(());
                            } else {
                                println!("{:?} != {:?}", lhs.tp, rhs.tp);
                                panic!();
                            }
                        }
                    }
                    if lhs.tp == rhs.tp {
                        Ok(())
                    } else {
                        unreachable!()
                    }
                }
                Return(func, ret_value) => {
                    let func = self.ast.get_node(*func).tp.as_ref().unwrap();
                    match &func.tp {
                        Fn(_, func_ret) => match &**func_ret {
                            NodeType::Void => {
                                if let Some(ret_id) = ret_value {
                                    Err(self.ast.error(
                                        "Return value should be of type void.",
                                        "Not allowed to return a value from here",
                                        vec![*ret_id],
                                    ))
                                } else {
                                    Ok(())
                                }
                            }
                            _ => {
                                if let Some(ret_id) = ret_value {
                                    let ret = self.ast.get_node(*ret_id).tp.as_ref().unwrap();
                                    if let NodeType::Fn(_, func_ret_tp) = &func.tp {
                                        if ret.tp == **func_ret_tp {
                                            Ok(())
                                        } else {
                                            Err(self.ast.error(
                                                &format!("Return statement does not return the right type, {:?} expected, {:?} provided", func.tp, ret.tp),
                                                "Wrong return type for function",
                                                vec![*ret_id],
                                            ))
                                        }
                                    } else {
                                        unimplemented!();
                                    }
                                } else {
                                    Err(self.ast.error(
                                        "Return value can not be of type void",
                                        "A value must be provided when returning from here",
                                        vec![node.id],
                                    ))
                                }
                            }
                        },
                        _ => unreachable!(),
                    }
                }
                Call(func, args) => {
                    let func = &self.ast.get_node(*func).tp.as_ref().unwrap().tp;
                    let args = args
                        .iter()
                        .map(|id| self.ast.get_node(*id).tp.as_ref().unwrap().tp.clone())
                        .collect();
                    let call = &match func {
                        Fn(_, ret) => Fn(args, ret.clone()),
                        Type(ret) => Fn(Vec::new(), ret.clone()),
                        _ => unreachable!(),
                    };
                    self.fits(node, func, call)?;
                    Ok(())
                }
                Unlinked(..) => Err(self.ast.error(
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

    fn fits(&self, node: &Node, hole: &NodeType, shape: &NodeType) -> Result<()> {
        use NodeType::*;
        let fits = match (&hole, &shape) {
            (&Any, &shape) if shape != &Void => true,
            (&Type(fields), &Fn(shape_args, shape_ret)) => {
                if shape_args.len() > 0 {
                    return Err(self.ast.error(
                        "Type constructor can not have arguments",
                        "",
                        vec![node.id],
                    ));
                }

                if fields != shape_ret {
                    return Err(self.ast.error(
                        "type and constructor does not have the same number of types",
                        "",
                        vec![node.id],
                    ));
                }
                true
            }
            (&Fn(hole_args, hole_ret), &Fn(shape_args, shape_ret)) => {
                self.fits(node, &*hole_ret, &*shape_ret)?;
                if hole_args.len() < shape_args.len() {
                    false
                } else {
                    let mut vararg = None;
                    for (i, shape_arg) in shape_args.iter().enumerate() {
                        let hole_arg = if let Some(decl_arg) = vararg {
                            decl_arg
                        } else {
                            match &hole_args[i] {
                                VarArg(tp) => {
                                    vararg = Some(tp);
                                    tp
                                }
                                decl_arg => decl_arg,
                            }
                        };
                        self.fits(node, hole_arg, shape_arg)?;
                    }
                    true
                }
            }
            (&hole, &shape) if hole == shape => true,
            _ => false,
        };
        if fits {
            Ok(())
        } else {
            Err(self.ast.error(
                &format!(
                    "arguments are of the wrong type, {:?} expected, {:?} provided",
                    hole, shape
                ),
                "Wrong argument type",
                vec![node.id],
            ))
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
