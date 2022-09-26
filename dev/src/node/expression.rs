use crate::node::{NodeID, NodeIterator, NodeType, Reference};
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

impl Node for Function {}

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

impl Node for Operation {
    fn node_type(node_id: NodeID<Self>, ast: &Ast) -> Result<NodeType> {
        let op: &Self = ast.get_inner(node_id);
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

    fn link(_: NodeID<Self>, _: &mut Ast) -> Result<()> {
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

impl Node for Value {
    fn node_type(node_id: NodeID<Self>, ast: &Ast) -> Result<NodeType> {
        let value: &Value = ast.get_inner(node_id);
        Ok(match value {
            Value::Int(_) => NodeType::Int,
            Value::Float(_) => NodeType::Float,
            Value::String(_) => NodeType::String,
        })
    }
}

#[derive(Debug)]
pub struct VariableValue(State<String, NodeID<Reference>>);

impl VariableValue {
    pub fn new(state: State<String, NodeID<Reference>>) -> Self {
        Self(state)
    }
}

impl Node for VariableValue {
    fn node_type(node_id: NodeID<Self>, ast: &Ast) -> Result<NodeType> {
        let variable: &Self = ast.get_inner(node_id);
        match variable.0 {
            State::Linked(var) => ast.get_node_type(var),
            _ => Err(Error::TypeNotInferred),
        }
    }

    fn link(node_id: NodeID<Self>, ast: &mut Ast) -> Result<()> {
        let node: &Self = ast.get_inner(node_id);
        if let State::Unlinked(var) = &node.0 {
            let var = ast
                .closest_variable(node_id, var)?
                .ok_or(Error::VariableNotFound)?;

            let node: &mut Self = ast.get_inner_mut(node_id);
            node.0 = State::Linked(var);
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct StaticVariableValue(State<String, NodeID<Reference>>);

impl StaticVariableValue {
    pub fn new(state: State<String, NodeID<Reference>>) -> Self {
        Self(state)
    }
}

impl Node for StaticVariableValue {}
