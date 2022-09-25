use crate::node::{Expression, Node, NodeID, NodeType, Variable};
use crate::{impl_enum_node, Ast, Error, Result, State};
use std::iter;

impl_enum_node!(
    pub enum Statement {
        VariableDeclaration,
        VariableAssignment,
    }
);

impl Statement {
    pub fn variable(&self) -> Option<NodeID<Variable>> {
        match self {
            Statement::VariableDeclaration(dec) => Some(dec.variable),
            Statement::VariableAssignment(_) => None,
        }
    }

    pub fn value(&self) -> Option<NodeID<Expression>> {
        match self {
            Statement::VariableDeclaration(var) => Some(var.value),
            Statement::VariableAssignment(var) => Some(var.value),
        }
    }
}

#[derive(Debug)]
pub struct VariableDeclaration {
    variable: NodeID<Variable>,
    pub value: NodeID<Expression>,
}

impl VariableDeclaration {
    pub fn new(variable: NodeID<Variable>, value: NodeID<Expression>) -> Self {
        VariableDeclaration { variable, value }
    }

    pub fn children(&self) -> Box<dyn Iterator<Item = &NodeID> + '_> {
        let variable_iter = iter::once(&self.variable).map(|c| c.into());
        let value_iter = iter::once(&self.value).map(|c| c.into());
        Box::new(variable_iter.chain(value_iter))
    }

    fn node_type(_: NodeID<Statement>, _: &Ast) -> Result<NodeType> {
        Ok(NodeType::Void)
    }

    pub fn link(_: NodeID<Statement>, _: &mut Ast) -> Result<()> {
        Ok(())
    }
}

#[derive(Debug)]
pub struct VariableAssignment {
    variable: State<String, NodeID<Variable>>,
    value: NodeID<Expression>,
}

impl VariableAssignment {
    pub fn new(ident: String, value: NodeID<Expression>) -> Self {
        VariableAssignment {
            variable: State::Unlinked(ident),
            value,
        }
    }

    pub fn children(&self) -> Box<dyn Iterator<Item = &NodeID> + '_> {
        Box::new(iter::once(&self.value).map(|c| c.into()))
    }

    fn node_type(_: NodeID<Statement>, _: &Ast) -> Result<NodeType> {
        Ok(NodeType::Void)
    }

    pub fn link(node_id: NodeID<Statement>, ast: &mut Ast) -> Result<()> {
        let node: &Self = ast.get_inner(node_id).try_into()?;
        if let State::Unlinked(var) = &node.variable {
            let var = ast
                .closest_variable(node_id, var)?
                .ok_or(Error::VariableNotFound)?;

            let node: &mut Self = ast.get_inner_mut(node_id).try_into()?;
            node.variable = State::Linked(var);
        }
        Ok(())
    }
}
