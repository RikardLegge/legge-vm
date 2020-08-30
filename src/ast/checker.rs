use super::{Ast, Node, Result};
use crate::ast::{NodeBody, NodeType};

pub fn check_types(ast: &Ast) -> Result<()> {
    Checker::new(ast).check_all_types()
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
                VariableAssignment(lhs, rhs) => {
                    let lhs_tp = self.ast.get_node(*lhs).tp.as_ref().unwrap();
                    let rhs_tp = self.ast.get_node(*rhs).tp.as_ref().unwrap();
                    if lhs_tp.tp == rhs_tp.tp {
                        Ok(())
                    } else {
                        Err(self.ast.error(
                            &format!("Types for left and right hand side of assignment do not match ({:?} != {:?})",                                 lhs_tp.tp, rhs_tp.tp
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
                                    if ret.tp == func.tp {
                                        Err(self.ast.error(
                                            &format!("Return statement does not return the right type, {:?} expected, {:?} provided", func.tp, ret.tp),
                                            "Wrong return type for function",
                                            vec![*ret_id],
                                        ))
                                    } else {
                                        Ok(())
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
                    let func = self.ast.get_node(*func).tp.as_ref().unwrap();
                    match &func.tp {
                        Fn(decl_args, _) => {
                            let mut vararg = None;
                            for (i, arg_id) in args.iter().enumerate() {
                                let arg = &self.ast.get_node(*arg_id).tp.as_ref().unwrap().tp;
                                let decl_arg = if let Some(arg) = vararg {
                                    arg
                                } else {
                                    let arg = &decl_args[i];
                                    match arg {
                                        VarArg(tp) => {
                                            vararg = Some(tp);
                                            tp
                                        }
                                        _ => arg,
                                    }
                                };
                                if decl_arg != arg && decl_arg != &Any {
                                    return Err(self.ast.error(
                                        &format!("Function call arguments are of the wrong type, {:?} expected, {:?} provided", decl_arg, arg),
                                        "Wrong argument type",
                                        vec![node.id],
                                    ));
                                }
                            }
                            Ok(())
                        }
                        _ => unreachable!(),
                    }
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

    pub fn check_all_types(&self) -> Result<()> {
        let root_id = self.ast.root();
        let root = self.ast.get_node(root_id);
        self.check_type(root)?;
        for child_id in root.body.children() {
            let child = self.ast.get_node(*child_id);
            self.check_type(child)?;
        }
        Ok(())
    }
}
