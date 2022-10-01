use crate::ast::AstContext;
use crate::node::iterator::NodeIteratorBody;
use crate::node::{NodeID, NodeIDContext, NodeIterator, NodeType, NodeUsage, Variable};
use crate::token::ArithmeticOP;
use crate::{Ast, Error, Expression, Result};
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
}

impl Node for Value {
    fn node_type(node_id: NodeID<Self>, ast: &Ast, _node_usage: NodeUsage) -> Result<NodeType> {
        let value: &Value = ast.get_body(node_id);
        Ok(match value {
            Value::Int(_) => NodeType::Int,
            Value::Float(_) => NodeType::Float,
            Value::String(_) => NodeType::String,
        })
    }
}

#[derive(Debug, Clone)]
pub struct FunctionCall {
    pub variable: NodeID<VariableValue>,
}

impl FunctionCall {
    pub fn new(variable: NodeID<VariableValue>) -> Self {
        Self { variable }
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

    fn children(&self, context: AstContext) -> NodeIterator<'_> {
        NodeIterator::new(NodeIteratorBody::Single(NodeIDContext {
            node_id: self.variable.into(),
            context,
        }))
    }
}

#[derive(Copy, Clone, Debug)]
pub struct ExpressionChain {
    pub lhs: NodeID<Expression>,
    pub rhs: NodeID<Expression>,
}

impl ExpressionChain {
    pub fn new(lhs: NodeID<Expression>, rhs: NodeID<Expression>) -> Self {
        Self { lhs, rhs }
    }
}

impl Node for ExpressionChain {
    fn node_type(node_id: NodeID<Self>, ast: &Ast, usage: NodeUsage) -> Result<NodeType> {
        let body = ast.get_body(node_id);
        ast.get_node_type(body.rhs, usage)
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
            _ => Err(Error::TypeNotInferred),
        }
    }

    fn link(node_id: NodeID<Self>, ast: &mut Ast, context: AstContext) -> Result<()> {
        let body: &Self = ast.get_body(node_id);
        if let State::Unlinked(var) = &body.variable {
            let var = ast
                .closest_variable(node_id, var, context)?
                .ok_or_else(|| panic!())?; //Error::VariableNotFound(var.into()))?;

            let body: &mut Self = ast.get_inner_mut(node_id);
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
