use crate::node::{NodeID, NodeType, Variable};
use crate::token::ArithmeticOP;
use crate::{impl_enum_node, Ast, Error, Result};
use crate::{Node, State};
use std::iter;

impl_enum_node!(
    pub enum Expression {
        ConstValue,
        VariableValue,
        Operation,
    }
);

#[derive(Debug)]
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

    fn link(_: NodeID<Expression>, _: &mut Ast) -> Result<()> {
        Ok(())
    }

    fn children(&self) -> Box<dyn Iterator<Item = &NodeID> + '_> {
        let lhs_iter = iter::once(&self.lhs).map(|c| c.into());
        let rhs_iter = iter::once(&self.rhs).map(|c| c.into());
        Box::new(lhs_iter.chain(rhs_iter))
    }

    pub fn node_type(node_id: NodeID<Expression>, ast: &Ast) -> Result<NodeType> {
        let op: &Self = ast.get_inner(node_id).try_into()?;
        let lhs_type = ast.get_node_type(op.lhs)?;
        let rhs_type = ast.get_node_type(op.lhs)?;
        if lhs_type == rhs_type {
            Ok(lhs_type)
        } else {
            unimplemented!()
        }
    }
}

pub type ConstValue = Value;

#[derive(Debug)]
pub enum Value {
    Int(isize),
    Float(f64),
    String(String),
}

impl Value {
    pub fn node_type(node_id: NodeID<Expression>, ast: &Ast) -> Result<NodeType> {
        let value: &Value = ast.get_inner(node_id).try_into()?;
        Ok(match value {
            Value::Int(_) => NodeType::Int,
            Value::Float(_) => NodeType::Float,
            Value::String(_) => NodeType::String,
        })
    }

    fn link(_: NodeID<Expression>, _: &mut Ast) -> Result<()> {
        Ok(())
    }

    fn children(&self) -> Box<dyn Iterator<Item = &NodeID> + '_> {
        Box::new([].iter())
    }
}

#[derive(Debug)]
pub struct VariableValue(State<String, NodeID<Variable>>);

impl VariableValue {
    pub fn new(state: State<String, NodeID<Variable>>) -> Self {
        Self(state)
    }

    fn node_type(node_id: NodeID<Expression>, ast: &Ast) -> Result<NodeType> {
        let variable: &Self = ast.get_inner(node_id).try_into()?;
        match variable.0 {
            State::Linked(var) => ast.get_node_type(var),
            _ => Err(Error::TypeNotInferred),
        }
    }

    fn children(&self) -> Box<dyn Iterator<Item = &NodeID> + '_> {
        Box::new([].iter())
    }

    fn link(node_id: NodeID<Expression>, ast: &mut Ast) -> Result<()> {
        let node: &Self = ast.get_inner(node_id).try_into()?;
        if let State::Unlinked(var) = &node.0 {
            let var = ast
                .closest_variable(node_id, var)?
                .ok_or(Error::VariableNotFound)?;

            let node: &mut Self = ast.get_inner_mut(node_id).try_into()?;
            node.0 = State::Linked(var);
        }
        Ok(())
    }
}
