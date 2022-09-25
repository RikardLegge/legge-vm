use crate::node::{NodeID, NodeIterator, NodeType, Variable};
use crate::token::ArithmeticOP;
use crate::{impl_enum_node, Ast, Error, Result};
use crate::{Node, State};

impl_enum_node!(
    pub enum Expression {
        ConstValue,
        VariableValue,
        Operation,
        Function,
    }
);

#[derive(Debug)]
pub struct Function {}

impl Node<Expression> for Function {}

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
}

impl Node<Expression> for Operation {
    fn node_type(node_id: NodeID<Expression>, ast: &Ast) -> Result<NodeType> {
        let op: &Self = ast.get_inner(node_id).try_into()?;
        let lhs_type = ast.get_node_type(op.lhs)?;
        let rhs_type = ast.get_node_type(op.lhs)?;
        if lhs_type == rhs_type {
            Ok(lhs_type)
        } else {
            unimplemented!()
        }
    }

    fn children(&self) -> NodeIterator<'_> {
        NodeIterator::dual(self.lhs, self.rhs)
    }

    fn link(_: NodeID<Expression>, _: &mut Ast) -> Result<()> {
        Ok(())
    }
}

pub type ConstValue = Value;

#[derive(Debug)]
pub enum Value {
    Int(isize),
    Float(f64),
    String(String),
}

impl Node<Expression> for Value {
    fn node_type(node_id: NodeID<Expression>, ast: &Ast) -> Result<NodeType> {
        let value: &Value = ast.get_inner(node_id).try_into()?;
        Ok(match value {
            Value::Int(_) => NodeType::Int,
            Value::Float(_) => NodeType::Float,
            Value::String(_) => NodeType::String,
        })
    }
}

#[derive(Debug)]
pub struct VariableValue(State<String, NodeID<Variable>>);

impl VariableValue {
    pub fn new(state: State<String, NodeID<Variable>>) -> Self {
        Self(state)
    }
}

impl Node<Expression> for VariableValue {
    fn node_type(node_id: NodeID<Expression>, ast: &Ast) -> Result<NodeType> {
        let variable: &Self = ast.get_inner(node_id).try_into()?;
        match variable.0 {
            State::Linked(var) => ast.get_node_type(var),
            _ => Err(Error::TypeNotInferred),
        }
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
