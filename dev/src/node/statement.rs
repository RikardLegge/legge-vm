use crate::node::{Expression, Node, NodeID, NodeType, Variable};
use crate::{Ast, Result};
use std::iter;

#[derive(Debug)]
pub enum Statement {
    VariableDeclaration(VariableDeclaration),
    VariableAssignment(VariableAssignment),
}

impl Statement {
    pub fn variable(&self) -> Option<NodeID<Variable>> {
        match self {
            Statement::VariableDeclaration(dec) => Some(dec.variable),
            Statement::VariableAssignment(_) => None,
        }
    }

    pub fn variable_type(&self, ast: &Ast) -> Result<NodeType> {
        match self {
            Statement::VariableDeclaration(dec) => ast.get_node_type(dec.value),
            Statement::VariableAssignment(_) => unimplemented!(),
        }
    }
}

impl Node for Statement {
    fn node_type(_: NodeID<Self>, _: &Ast) -> Result<NodeType> {
        Ok(NodeType::Void)
    }

    fn children(&self) -> Box<dyn Iterator<Item = &NodeID> + '_> {
        match &self {
            Statement::VariableDeclaration(VariableDeclaration { variable, value }) => {
                let variable_iter = iter::once(variable).map(|c| c.into());
                let value_iter = iter::once(value).map(|c| c.into());
                Box::new(variable_iter.chain(value_iter))
            }
            Statement::VariableAssignment(VariableAssignment { name: _, value }) => {
                Box::new(iter::once(value).map(|c| c.into()))
            }
        }
    }
}

impl VariableDeclaration {
    pub fn new(variable: NodeID<Variable>, value: NodeID<Expression>) -> Self {
        VariableDeclaration { variable, value }
    }
}

impl From<VariableDeclaration> for Statement {
    fn from(var: VariableDeclaration) -> Self {
        Statement::VariableDeclaration(var)
    }
}

#[derive(Debug)]
pub struct VariableDeclaration {
    variable: NodeID<Variable>,
    value: NodeID<Expression>,
}

#[derive(Debug)]
pub struct VariableAssignment {
    name: String,
    value: NodeID<Expression>,
}

impl VariableAssignment {
    pub fn new(name: String, value: NodeID<Expression>) -> Self {
        VariableAssignment { name, value }
    }
}

impl From<VariableAssignment> for Statement {
    fn from(var: VariableAssignment) -> Self {
        Statement::VariableAssignment(var)
    }
}
