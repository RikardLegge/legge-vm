use super::{Ast, Node, Result};
use crate::ast;
use crate::ast::ast::PartialType::Complete;
use crate::ast::ast::{AstCollection, StateTypesChecked, TypesInferred};
use crate::ast::nodebody::{NBCall, NBProcedureDeclaration, NodeBody};
use crate::ast::{Err, ErrPart, NodeID, NodeType};
use std::result;
use std::sync::Arc;

pub fn check_types<T: TypesInferred>(
    asts: AstCollection<T>,
    runtime: &tokio::runtime::Runtime,
) -> result::Result<AstCollection<StateTypesChecked>, (AstCollection<T>, Err)>
where
    T: TypesInferred + 'static,
{
    runtime.block_on(async {
        let (tx, mut rx) = tokio::sync::mpsc::unbounded_channel();

        let asts = Arc::new(asts);
        for ast_id in (&asts).ids() {
            let asts = asts.clone();
            let tx = tx.clone();
            tokio::task::spawn_blocking(move || {
                let ast = asts.get(ast_id).read().unwrap();
                let root_id = ast.root();
                let checker = Checker::new(&ast, &asts);

                let result = checker.check_all_types(root_id);
                tx.send(result).unwrap();
            });
        }
        drop(tx);

        let mut errors = vec![];
        while let Some(msg) = rx.recv().await {
            if let Err(err) = msg {
                errors.push(err);
            }
        }

        let asts = Arc::try_unwrap(asts).unwrap();

        if let Some(err) = errors.pop() {
            Err((asts, err))
        } else {
            Ok(asts.guarantee_state())
        }
    })
}

pub struct Checker<'a, T>
where
    T: TypesInferred,
{
    ast: &'a Ast<T>,
    asts: &'a AstCollection<T>,
}

impl<'a, T> Checker<'a, T>
where
    T: TypesInferred,
{
    pub fn new(ast: &'a Ast<T>, asts: &'a AstCollection<T>) -> Self {
        Self { ast, asts }
    }

    pub fn check_type(&self, node: &Node<T>) -> Result<()> {
        use crate::ast::nodebody::NodeBody::*;
        use NodeType::*;
        match &node.body {
            Reference { node_id } => self.check_type(&*self.asts.get_node(*node_id)),
            Empty
            | Break { .. }
            | Comment { .. }
            | ConstValue { .. }
            | PrefixOp { .. }
            | Block { .. }
            | Loop { .. }
            | Expression { .. }
            | VariableValue { .. }
            | ProcedureDeclaration(NBProcedureDeclaration { .. })
            | TypeReference { .. }
            | PartialType { .. }
            | Import { .. } => Ok(()),
            Op { op, lhs, rhs } => {
                let lhs_tp = self.ast.get_node(*lhs).tp();
                let rhs_tp = self.ast.get_node(*rhs).tp();
                if lhs_tp == rhs_tp {
                    Ok(())
                } else {
                    Err(ast::Err::single(
                        &format!(
                            "Types for left and right hand side of op {} do not match ({} != {})",
                            op, lhs_tp, rhs_tp
                        ),
                        "Both sides must have the same type",
                        vec![node.id(), *lhs, *rhs],
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
                        Err(ast::Err::single(
                            "Not allowed to assign to constant value",
                            "Assignment to constant value",
                            vec![node.id()],
                        ))?
                    }
                    _ => (),
                }
                let mut variable_tp = variable_node.tp();
                if let Some(path) = path {
                    for path_field in path {
                        let fields = if let NodeType::Struct { fields } = &variable_tp {
                            fields
                        } else if let NodeType::Type { content, .. } = &variable_tp {
                            if let NodeType::Struct { fields } = &**content {
                                fields
                            } else {
                                unimplemented!("{:?}", variable_tp)
                            }
                        } else {
                            unimplemented!("{:?}", variable_tp)
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
                                variable_tp = tp;
                            }
                            None => {
                                return Err(ast::Err::single(
                                    &format!("Struct does not have the field '{}'", path_field),
                                    "",
                                    vec![node.id()],
                                ));
                            }
                        }
                    }
                }
                let expr_tp = self.ast.get_node(*expr).tp();
                if variable_tp == expr_tp {
                    Ok(())
                } else {
                    Err(ast::Err::single(
                            &format!("Types for left and right hand side of assignment do not match ({:?} != {:?})", variable_tp, expr_tp
                            ),
                            "Both sides of an assignment must have the same type",
                            vec![self.ast.get_node(*expr).parent_id.unwrap(), *expr],
                        ))
                }
            }
            If { condition, .. } => match self.ast.get_node(*condition).tp() {
                Bool => Ok(()),
                tp => Err(ast::Err::single(
                    &format!(
                        "The condition of an if statement must be of type Bool, ({:?}) was found",
                        tp
                    ),
                    "Both sides of an assignment must have the same type",
                    vec![*condition],
                )),
            },
            VariableDeclaration { expr, .. } => {
                if let Some(expr) = expr {
                    let lhs = node.tp();
                    let rhs = self.ast.get_node(*expr).tp();
                    assert_eq!(lhs, rhs)
                }
                Ok(())
            }
            TypeDeclaration { constructor, .. } => {
                let lhs = node.tp();
                let rhs = self.ast.get_node(**constructor).tp();
                match (lhs, rhs) {
                    (NewType { tp, .. }, Fn { returns, .. }) => {
                        assert_eq!(tp, returns)
                    }
                    _ => unreachable!(),
                }
                Ok(())
            }

            ConstDeclaration { expr, .. } | StaticDeclaration { expr, .. } => {
                let lhs = node.tp();
                let rhs = if expr.ast() == self.ast.id() {
                    let node = self.ast.get_node(*expr);
                    node.tp()
                } else {
                    self.asts.get_node(*expr);
                    node.tp()
                };
                if lhs != rhs {
                    Err(ast::Err::single(
                        &format!(
                            "Left and right hand side must have the same types, '{:?}' != '{:?}'",
                            lhs, rhs
                        ),
                        "",
                        vec![*expr, self.ast.get_node(*expr).parent_id.unwrap()],
                    ))?
                }
                Ok(())
            }
            Return {
                func,
                expr,
                automatic,
            } => {
                let func = self.ast.get_node(*func);
                let func_tp = func.tp();
                if let Fn { returns, .. } = &func_tp {
                    match (&**returns, expr) {
                        (NodeType::Void, None) => Ok(()),
                        (NodeType::Void, Some(ret_id)) => Err(ast::Err::single(
                            "Return value should be of type void.",
                            "Not allowed to return a value from here",
                            vec![*ret_id],
                        )),
                        (expected, None) => {
                            if *automatic {
                                let mut nodes = vec![];
                                let body_id = **func
                                    .body
                                    .children()
                                    .collect::<Vec<&NodeID>>()
                                    .last()
                                    .unwrap();
                                nodes.push(body_id);
                                let statements = self
                                    .ast
                                    .get_node(body_id)
                                    .body
                                    .children()
                                    .cloned()
                                    .collect::<Vec<NodeID>>();

                                // The last statement will be the automatically inserted return statement.
                                if statements.len() >= 2 {
                                    let last_node_id = statements[statements.len() - 2];
                                    nodes.push(last_node_id);
                                }

                                Err(ast::Err::single(
                                        "Function missing return statement",
                                        &format!("A return of type {:?} must be provided as the final statement of the function", expected),
                                       nodes
                                    ))
                            } else {
                                Err(ast::Err::single(
                                    "Return value can not be of type void",
                                    "A value must be provided when returning from here",
                                    vec![node.id()],
                                ))
                            }
                        }
                        (tp, Some(ret_id)) => {
                            let ret = self.ast.get_node(*ret_id).tp();
                            if ret != tp {
                                let mut parts = vec![ErrPart::new(
                                    "Wrong return type for function".into(),
                                    self.ast.get_node_and_children(*ret_id),
                                )];
                                if let NodeBody::ProcedureDeclaration(NBProcedureDeclaration {
                                    returns: Some(return_id),
                                    ..
                                }) = func.body
                                {
                                    parts.push(ErrPart::new(
                                        "Expected return type".into(),
                                        self.ast.get_node_and_children(return_id),
                                    ));
                                };
                                let details = format!("Return statement does not return the right type, {:?} expected, {:?} provided", func.tp(), ret);
                                Err(ast::Err::new(details, parts))?
                            }
                            Ok(())
                        }
                    }
                } else {
                    unreachable!()
                }
            }
            Call(NBCall { func, args }) => {
                let caller = node;
                let func = self.ast.get_node(*func);
                let args = args
                    .iter()
                    .map(|id| self.ast.get_node(*id).tp().clone())
                    .collect();

                let func_tp = func.tp();
                let returns = match func_tp {
                    Fn { returns, .. } | NewType { tp: returns, .. } => returns.clone(),
                    _ => unreachable!(),
                };

                let call = Fn { args, returns };
                self.fits_function_call(func, caller, func_tp, &call)?;
                Ok(())
            }
            Unlinked { .. } => Err(ast::Err::single(
                "Encountered a node with unlinked type",
                "expression with unlinked type",
                vec![node.id()],
            )),
        }
    }

    fn correct_number_of_arguments<'b>(
        &'_ self,
        hole: &'b Node<T>,
        shape: &'b Node<T>,
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
            let invalid_node = shape.body.children().skip(i).next();
            if let Some(invalid_node) = invalid_node {
                Err(ast::Err::single(
                    &format!("miss placed vararg",),
                    "Invalid vararg",
                    vec![*invalid_node],
                ))
            } else {
                Err(ast::Err::single(
                    &format!("missplaced vararg",),
                    "Too many arguments",
                    vec![shape.id()],
                ))
            }
        } else if hole_args_len < shape_args.len() {
            if let Some(NodeType::VarArg { args }) = hole_args.last() {
                Ok(Some(&*args))
            } else {
                let skip = hole_args.len();
                let mut nodes = shape
                    .body
                    .children()
                    .skip(skip)
                    .map(|id| *id)
                    .collect::<Vec<NodeID>>();
                if nodes.len() == 0 {
                    nodes.push(shape.id());
                }
                nodes.push(hole.id());
                Err(ast::Err::single(
                    &format!(
                        "wrong number of arguments, {} expected, {} provided",
                        hole_args.len(),
                        shape_args.len()
                    ),
                    "Too many arguments",
                    nodes,
                ))
            }
        } else if hole_args_len > shape_args.len() {
            let mut nodes = shape.body.children().map(|id| *id).collect::<Vec<NodeID>>();
            if nodes.len() == 0 {
                nodes.push(shape.id());
            }
            Err(ast::Err::single(
                &format!(
                    "wrong number of arguments, {} expected, {} provided",
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

    fn get_function_trace(&self, referer: NodeID, depth: usize) -> Vec<NodeID> {
        let mut trace = vec![referer];
        match &self.asts.get_node(referer).body {
            NodeBody::ProcedureDeclaration(NBProcedureDeclaration {
                returns: Some(id), ..
            }) => {
                if depth > 0 {
                    trace = self.get_function_trace(*id, depth - 1);
                }
            }
            NodeBody::Call(NBCall { func: child, .. }) => {
                trace.append(&mut self.get_function_trace(*child, depth + 1))
            }
            NodeBody::StaticDeclaration { expr: child, .. } => {
                trace.append(&mut self.get_function_trace(*child, depth))
            }
            NodeBody::Reference { node_id: child, .. } => {
                trace.append(&mut self.get_function_trace(*child, depth))
            }
            NodeBody::PartialType { .. } => {}
            body => unimplemented!("{:?}", body),
        };
        trace
    }

    fn fits_function_argument(
        &self,
        arg_i: Option<usize>,
        func: &Node<T>,
        caller: &Node<T>,
        hole: &NodeType,
        shape: &NodeType,
    ) -> Result<()> {
        use NodeType::*;
        match (hole, shape) {
            // Flip shape and hole since we are comparing types now!
            (Fn { .. }, Fn { .. }) => self.fits_function_function(func, caller, shape, hole),
            (Any, Void) if *shape != Void => Err(ast::Err::single(
                "function expected Any type of value, nothing (Void) provided.",
                "Argument required here",
                vec![caller.id(), func.id()],
            )),
            (Any, _) => Ok(()),
            (hole, shape) if hole == shape => Ok(()),
            _ => {
                let trace_ids = self.get_function_trace(func.id(), 0);
                let mut error_parts =
                    vec![ErrPart::new("Provided argument".into(), vec![caller.id()])];

                for func_id in trace_ids {
                    match &self.asts.get_node(func_id).body {
                        NodeBody::Call(_) => error_parts.push(ErrPart::new(
                            "From return value of function".into(),
                            vec![func_id],
                        )),

                        NodeBody::Reference { .. } => {
                            error_parts.push(ErrPart::new("Imported here".into(), vec![func_id]))
                        }

                        NodeBody::ProcedureDeclaration(NBProcedureDeclaration {
                            args,
                            returns,
                            ..
                        }) => error_parts.push(match arg_i {
                            Some(i) => ErrPart::new("Expected argument type".into(), vec![args[i]]),
                            None => match returns {
                                None => {
                                    ErrPart::new("Expected void return value".into(), vec![func_id])
                                }
                                Some(return_id) => {
                                    ErrPart::new("Expected return type".into(), vec![*return_id])
                                }
                            },
                        }),

                        NodeBody::PartialType {
                            tp: Complete(Fn { returns, .. }),
                            parts,
                        } => error_parts.push(match arg_i {
                            Some(i) => {
                                ErrPart::new("Expected argument type".into(), vec![parts[i]])
                            }
                            None => match &**returns {
                                Void => {
                                    ErrPart::new("Expected void return value".into(), vec![func_id])
                                }
                                _ => ErrPart::new(
                                    "Expected return".into(),
                                    vec![*parts.last().unwrap()],
                                ),
                            },
                        }),
                        _ => {}
                    };
                }
                Err(ast::Err::new(
                    format!(
                        "function arguments are of the wrong type, {} expected, {} provided",
                        hole, shape
                    ),
                    error_parts,
                ))
            }
        }
    }

    fn fits_function_function(
        &self,
        func: &Node<T>,
        caller: &Node<T>,
        hole: &NodeType,
        shape: &NodeType,
    ) -> Result<()> {
        match (hole, shape) {
            (
                NodeType::Fn {
                    args: hole_args,
                    returns: hole_ret,
                },
                NodeType::Fn {
                    args: shape_args,
                    returns: shape_ret,
                },
            ) => {
                let mut vararg_iter;
                let mut iter;

                self.fits_function_argument(None, func, caller, hole_ret, shape_ret)?;
                let vararg_tp =
                    self.correct_number_of_arguments(func, caller, &hole_args, &shape_args)?;
                let caller_arg_ids = caller
                    .body
                    .children()
                    .map(|id| *id)
                    .collect::<Vec<NodeID>>();
                let func_arg_ids = func.body.children().map(|id| *id).collect::<Vec<NodeID>>();
                let args: &mut dyn Iterator<Item = (usize, (&NodeType, &NodeType))> =
                    if vararg_tp.is_some() {
                        vararg_iter = shape_args.iter().enumerate().map(|(i, shape)| {
                            let hole = if i < hole_args.len() - 1 {
                                &hole_args[i]
                            } else {
                                vararg_tp.unwrap()
                            };
                            (i, (hole, shape))
                        });
                        &mut vararg_iter
                    } else {
                        iter = hole_args.iter().zip(shape_args).enumerate();
                        &mut iter
                    };
                for (i, (hole, shape)) in args {
                    let caller_arg = if i >= caller_arg_ids.len() {
                        // Only used for printing errors, so ok if we do not have a correct id
                        caller
                    } else {
                        self.ast.get_node(caller_arg_ids[i])
                    };
                    let func_arg = if i >= func_arg_ids.len() {
                        // Only used for printing errors, so ok if we do not have a correct id
                        func
                    } else {
                        self.ast.get_node(func_arg_ids[i])
                    };
                    self.fits_function_argument(Some(i), func_arg, caller_arg, hole, shape)?;
                }

                Ok(())
            }
            _ => unreachable!(),
        }
    }

    fn fits_function_call(
        &self,
        func: &Node<T>,
        caller: &Node<T>,
        hole: &NodeType,
        shape: &NodeType,
    ) -> Result<()> {
        use NodeType::*;
        match (hole, shape) {
            (NewType { tp: fields, .. }, Fn { args, returns }) => {
                if args.len() > 0 {
                    let nodes = caller.body.children().map(|id| *id).collect();
                    Err(ast::Err::single(
                        "Type constructor can not have arguments",
                        "Arguments not allowed here",
                        nodes,
                    ))
                } else if fields != returns {
                    Err(ast::Err::single(
                        "constructor does not return correct struct",
                        "",
                        vec![func.id()],
                    ))
                } else {
                    Ok(())
                }
            }
            (Fn { .. }, Fn { .. }) => self.fits_function_function(func, caller, hole, shape),
            (_, Fn { .. }) => Err(ast::Err::single(
                &format!("tried to call non-callable function"),
                "Can not call",
                vec![caller.id(), func.id()],
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
