use crate::node::NodeIterator;
use crate::node::{Expression, Node, NodeID, NodeType, Variable};
use crate::{impl_enum_node, Ast, Error, Result, State};
use std::collections::HashMap;

impl_enum_node!(
    pub enum Statement {
        VariableDeclaration,
        VariableAssignment,
        TypeDeclaration,
    }
);

impl Statement {
    pub fn variable(&self) -> Option<NodeID<Variable>> {
        match self {
            Statement::VariableDeclaration(dec) => Some(dec.variable),
            Statement::VariableAssignment(_) => None,
            Statement::TypeDeclaration(dec) => Some(dec.variable),
        }
    }

    pub fn value(&self) -> Option<NodeID<Expression>> {
        match self {
            Statement::VariableDeclaration(var) => Some(var.value),
            Statement::VariableAssignment(var) => Some(var.value),
            Statement::TypeDeclaration(var) => Some(var.constructor),
        }
    }
}

#[derive(Debug)]
pub struct TypeDeclaration {
    variable: NodeID<Variable>,
    constructor: NodeID<Expression>,
    fields: Vec<NodeID>,
    associated_values: HashMap<String, NodeID<Expression>>,
}

impl TypeDeclaration {
    pub fn new(variable: NodeID<Variable>, constructor: NodeID<Expression>) -> Self {
        TypeDeclaration {
            variable,
            constructor,
            fields: Vec::new(),
            associated_values: HashMap::new(),
        }
    }
}

impl Node<Statement> for TypeDeclaration {
    fn children(&self) -> NodeIterator<'_> {
        let props = NodeIterator::dual(self.variable, self.constructor);
        let fields = NodeIterator::slice(&self.fields);
        NodeIterator::chained(props, fields)
    }
}

#[derive(Debug)]
pub struct VariableDeclaration {
    variable: NodeID<Variable>,
    value: NodeID<Expression>,
}

impl VariableDeclaration {
    pub fn new(variable: NodeID<Variable>, value: NodeID<Expression>) -> Self {
        VariableDeclaration { variable, value }
    }
}

impl Node<Statement> for VariableDeclaration {
    fn node_type(_: NodeID<Statement>, _: &Ast) -> Result<NodeType> {
        Ok(NodeType::Void)
    }

    fn children(&self) -> NodeIterator<'_> {
        NodeIterator::dual(self.variable, self.value)
    }

    fn link(_: NodeID<Statement>, _: &mut Ast) -> Result<()> {
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
}

impl Node<Statement> for VariableAssignment {
    fn node_type(_: NodeID<Statement>, _: &Ast) -> Result<NodeType> {
        Ok(NodeType::Void)
    }

    fn children(&self) -> NodeIterator<'_> {
        NodeIterator::single(self.value)
    }

    fn link(node_id: NodeID<Statement>, ast: &mut Ast) -> Result<()> {
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
