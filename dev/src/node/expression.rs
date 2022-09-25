use crate::node::{NodeID, NodeType, Variable};
use crate::token::ArithmeticOP;
use crate::{Ast, Error, Result};
use crate::{Node, State};
use std::iter;

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

    pub fn node_type(&self, ast: &Ast) -> Result<NodeType> {
        let lhs_type = ast.get_node_type(self.lhs)?;
        let rhs_type = ast.get_node_type(self.lhs)?;
        if lhs_type == rhs_type {
            Ok(lhs_type)
        } else {
            unimplemented!()
        }
    }
}

impl From<Operation> for Expression {
    fn from(op: Operation) -> Self {
        Expression::Operation(op)
    }
}

#[derive(Debug)]
pub enum Expression {
    ConstValue(Value),
    VariableValue(State<String, NodeID<Variable>>),
    Operation(Operation),
}

#[derive(Debug)]
pub enum Value {
    Int(isize),
    Float(f64),
    String(String),
}

impl Value {
    pub fn node_type(&self) -> Result<NodeType> {
        Ok(match &self {
            Value::Int(_) => NodeType::Int,
            Value::Float(_) => NodeType::Float,
            Value::String(_) => NodeType::String,
        })
    }
}

impl From<Value> for Expression {
    fn from(value: Value) -> Self {
        Expression::ConstValue(value)
    }
}

impl Node for Expression {
    fn node_type(node_id: NodeID<Self>, ast: &Ast) -> Result<NodeType> {
        let node = ast.get_inner(node_id);
        match node {
            Expression::ConstValue(value) => value.node_type(),
            Expression::VariableValue(State::Linked(var)) => ast.get_node_type(*var),
            Expression::Operation(operation) => operation.node_type(ast),
            _ => Err(Error::TypeNotInferred),
        }
    }

    fn children(&self) -> Box<dyn Iterator<Item = &NodeID> + '_> {
        match &self {
            Expression::ConstValue(_) => Box::new([].iter()),
            Expression::VariableValue(_) => Box::new([].iter()),
            Expression::Operation(Operation { lhs, rhs, .. }) => {
                let lhs_iter = iter::once(lhs).map(|c| c.into());
                let rhs_iter = iter::once(rhs).map(|c| c.into());
                Box::new(lhs_iter.chain(rhs_iter))
            }
        }
    }

    fn link(id: NodeID<Self>, ast: &mut Ast) -> Result<()> {
        let node = ast.get_inner(id);
        match &node {
            Expression::VariableValue(State::Unlinked(var)) => {
                let var = ast
                    .closest_variable(id, var)?
                    .ok_or(Error::VariableNotFound)?;
                let node = ast.get_inner_mut(id);
                *node = Expression::VariableValue(State::Linked(var));
                Ok(())
            }
            _ => Ok(()),
        }
    }
}
