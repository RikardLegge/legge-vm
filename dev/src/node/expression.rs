use crate::ast::AstContext;
use crate::node::iterator::NodeIteratorBody;
use crate::node::{
    NodeID, NodeIDContext, NodeIterator, NodeType, NodeUsage, TypeDeclaration, Variable,
};
use crate::token::ArithmeticOP;
use crate::{Ast, AstNode, Error, Expression, Result};
use crate::{Node, State};

#[derive(Debug, Clone)]
pub struct Operation {
    pub op: ArithmeticOP,
    pub lhs: NodeID<Expression>,
    pub rhs: NodeID<Expression>,
    pub tp: Option<NodeType>,
}

impl Operation {
    pub fn new(op: ArithmeticOP, lhs: NodeID<Expression>, rhs: NodeID<Expression>) -> Self {
        Operation {
            op,
            lhs,
            rhs,
            tp: None,
        }
    }
}

impl Node for Operation {
    fn node_type(node_id: NodeID<Self>, ast: &Ast, node_usage: NodeUsage) -> Result<NodeType> {
        match node_usage {
            NodeUsage::Value => {
                let op: &Self = ast.get_body(node_id);
                let lhs_type = ast.get_node_type(op.lhs, NodeUsage::Value)?;
                let rhs_type = ast.get_node_type(op.lhs, NodeUsage::Value)?;
                if lhs_type == rhs_type {
                    Ok(lhs_type)
                } else {
                    unimplemented!()
                }
            }
            _ => unimplemented!(),
        }
    }

    fn children(&self, _context: AstContext) -> NodeIterator<'_> {
        NodeIterator::dual(self.lhs, self.rhs)
    }

    fn link(_: NodeID<Self>, _: &mut Ast, _context: AstContext) -> Result<()> {
        Ok(())
    }
}

pub type ConstValue = Value;

#[derive(Debug, Clone)]
pub enum Value {
    Int(isize),
    Float(f64),
    String(String),
    Custom(NodeID<TypeDeclaration>),
}

impl Node for Value {
    fn node_type(node_id: NodeID<Self>, ast: &Ast, _node_usage: NodeUsage) -> Result<NodeType> {
        let value: &Value = ast.get_body(node_id);
        Ok(match value {
            Value::Int(_) => NodeType::Int,
            Value::Float(_) => NodeType::Float,
            Value::String(_) => NodeType::String,
            Value::Custom(id) => NodeType::Custom(*id),
        })
    }
}

#[derive(Debug, Clone)]
pub struct FunctionCall {
    pub variable: NodeID<VariableValue>,
    pub args: Vec<NodeID<Expression>>,
}

impl FunctionCall {
    pub fn new(variable: NodeID<VariableValue>, args: Vec<NodeID<Expression>>) -> Self {
        Self { variable, args }
    }
}

impl Node for FunctionCall {
    fn node_type(node_id: NodeID<Self>, ast: &Ast, node_usage: NodeUsage) -> Result<NodeType> {
        match node_usage {
            NodeUsage::Type => {
                let body = ast.get_body(node_id);
                ast.get_node_type(body.variable, node_usage)
            }
            NodeUsage::Call | NodeUsage::Value => {
                let body = ast.get_body(node_id);
                ast.get_node_type(body.variable, NodeUsage::Call)
            }
        }
    }

    fn check(node_id: NodeID<Self>, ast: &mut Ast) -> Result<()> {
        let call = ast.get_body(node_id);

        let var_id: NodeID<Variable> = (&ast.get_body(call.variable).variable)
            .try_into()
            .map_err(|_| Error::UnlinkedNode(call.variable.into()))?;
        let var_tp = ast.get_node_type(var_id, NodeUsage::Value)?;

        let func_id = match var_tp {
            NodeType::Function(func_id) => func_id,
            _ => return Err(Error::TypeMissmatch(node_id.into(), var_id.into())),
        };
        let func = ast.get_body(func_id);

        if func.arguments.len() != call.args.len() {
            return Err(Error::TypeMissmatch(node_id.into(), func_id.into()));
        }

        for (func_arg_id, call_arg_id) in func
            .arguments
            .iter()
            .map(ToOwned::to_owned)
            .zip(call.args.iter().map(ToOwned::to_owned))
        {
            let func_arg_tp = ast.get_node_type(func_arg_id, NodeUsage::Type)?;
            let call_arg_tp = ast.get_node_type(call_arg_id, NodeUsage::Type)?;

            if func_arg_tp != call_arg_tp {
                return Err(Error::TypeMissmatch(func_arg_id.into(), call_arg_id.into()));
            }
        }

        Ok(())
    }

    fn children(&self, context: AstContext) -> NodeIterator<'_> {
        let variable = NodeIterator::new(NodeIteratorBody::Single(NodeIDContext {
            node_id: self.variable.into(),
            context,
        }));
        let arguments = NodeIterator::slice(&self.args);
        NodeIterator::chained(variable, arguments)
    }
}

#[derive(Copy, Clone, Debug)]
pub struct ExpressionChain {
    pub lhs: NodeID<Expression>,
    pub rhs: NodeID<Expression>,
    linked: bool,
}

impl ExpressionChain {
    pub fn new(lhs: NodeID<Expression>, rhs: NodeID<Expression>) -> Self {
        Self {
            lhs,
            rhs,
            linked: false,
        }
    }
}

impl Node for ExpressionChain {
    fn node_type(node_id: NodeID<Self>, ast: &Ast, usage: NodeUsage) -> Result<NodeType> {
        let body = ast.get_body(node_id);
        ast.get_node_type(body.rhs, usage)
    }

    fn link(node_id: NodeID<Self>, ast: &mut Ast, _context: AstContext) -> Result<()> {
        let node = ast.get_body(node_id);

        if node.linked {
            return Ok(());
        }
        let lhs_id = node.lhs;
        let lhs = ast.get_body(lhs_id);
        let lhs_variable = match lhs {
            Expression::VariableValue(value) => {
                let variable_id: NodeID<Variable> = (&value.variable)
                    .try_into()
                    .map_err(|_| Error::UnlinkedNode(lhs_id.into()))?;

                <AstNode<Variable>>::variable_declaration_id(variable_id, ast).map(|_| variable_id)
            }
            _ => None,
        };

        // If the lhs of the expression is a variable, not a type for example,
        // then we want to insert the variable as an implicit first parameter in the call.
        if let Some(variable_id) = lhs_variable {
            let rhs = ast.get(node.rhs);
            if let Ok(rhs) = <&AstNode<FunctionCall>>::try_from(rhs) {
                let value =
                    ast.push_new_node(node.rhs, VariableValue::new(State::Linked(variable_id)));

                let call = ast.get_body_mut(rhs.id);
                call.args.insert(0, value.into());
            }
        }

        ast.get_body_mut(node_id).linked = true;
        Ok(())
    }

    fn children(&self, _context: AstContext) -> NodeIterator<'_> {
        NodeIterator::dual(
            self.lhs,
            NodeIDContext {
                node_id: self.rhs.into(),
                context: AstContext::Chain(self.lhs),
            },
        )
    }
}

#[derive(Debug, Clone)]
pub struct VariableValue {
    pub variable: State<String, NodeID<Variable>>,
}

impl VariableValue {
    pub fn new(variable: State<String, NodeID<Variable>>) -> Self {
        Self { variable }
    }
}

impl Node for VariableValue {
    fn node_type(node_id: NodeID<Self>, ast: &Ast, node_usage: NodeUsage) -> Result<NodeType> {
        let value: &Self = ast.get_body(node_id);
        match value.variable {
            State::Linked(var) => ast.get_node_type(var, node_usage),
            _ => Err(Error::UnlinkedNode(node_id.into())),
        }
    }

    fn link(node_id: NodeID<Self>, ast: &mut Ast, context: AstContext) -> Result<()> {
        let body: &Self = ast.get_body(node_id);
        if let State::Unlinked(var) = &body.variable {
            let var = ast
                .closest_variable(node_id, var, context)?
                .ok_or_else(|| panic!())?; //Error::VariableNotFound(var.into()))?;

            let body: &mut Self = ast.get_body_mut(node_id);
            body.variable = State::Linked(var);
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct StaticVariableValue(State<String, NodeID<Variable>>);

impl StaticVariableValue {
    pub fn new(state: State<String, NodeID<Variable>>) -> Self {
        Self(state)
    }
}

impl Node for StaticVariableValue {}
