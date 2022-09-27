use crate::node::NodeIterator;
use crate::node::{Expression, Node, NodeID, NodeType, Variable};
use crate::{impl_enum_node, Ast, AstNode, Error, Result, State};
use std::collections::HashMap;

impl_enum_node!(
    pub enum Statement {
        VariableDeclaration,
        VariableAssignment,
        StaticAssignment,
        TypeDeclaration,
        FunctionDeclaration,
    }
);

#[derive(Debug)]
pub struct FunctionDeclaration {
    pub arguments: Vec<NodeID<Variable>>,
    pub returns: NodeType,
}

impl Node for FunctionDeclaration {
    fn node_type(node_id: NodeID<Self>, _: &Ast) -> Result<NodeType> {
        Ok(NodeType::Function(node_id))
    }
}

impl Statement {
    pub fn variable(&self) -> Option<NodeID<Variable>> {
        match self {
            Statement::VariableDeclaration(dec) => Some(dec.variable),
            Statement::VariableAssignment(_) => None,
            Statement::TypeDeclaration(dec) => Some(dec.variable),
            Statement::StaticAssignment(_) => None,
            Statement::FunctionDeclaration(_) => None,
        }
    }

    pub fn value(&self) -> Option<NodeID> {
        match self {
            Statement::VariableDeclaration(var) => Some(var.value.into()),
            Statement::VariableAssignment(var) => Some(var.value.into()),
            Statement::TypeDeclaration(var) => Some(var.constructor.into()),
            Statement::StaticAssignment(_) => None,
            Statement::FunctionDeclaration(_) => None,
        }
    }
}

#[derive(Debug)]
pub struct TypeDeclaration {
    variable: NodeID<Variable>,
    constructor: NodeID<Statement>,
    fields: Vec<NodeID>,
    associated_values: HashMap<String, NodeID<Expression>>,
}

impl TypeDeclaration {
    pub fn new(variable: NodeID<Variable>, constructor: NodeID<Statement>) -> Self {
        TypeDeclaration {
            variable,
            constructor,
            fields: Vec::new(),
            associated_values: HashMap::new(),
        }
    }
}

impl Node for TypeDeclaration {
    fn node_type(_: NodeID<Self>, _: &Ast) -> Result<NodeType> {
        Ok(NodeType::Void)
    }

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

impl Node for VariableDeclaration {
    fn node_type(_: NodeID<Self>, _: &Ast) -> Result<NodeType> {
        Ok(NodeType::Void)
    }

    fn children(&self) -> NodeIterator<'_> {
        NodeIterator::dual(self.variable, self.value)
    }
}

#[derive(Debug)]
pub struct StaticAssignment {
    pub variable: State<String, NodeID<Variable>>,
    pub value: NodeID<Expression>,
    pub field: Option<String>,
    pub is_static: bool,
}

impl AstNode<StaticAssignment> {
    pub fn parent_id(&self) -> NodeID {
        self.parent_id.unwrap()
    }
}

impl Node for StaticAssignment {
    fn node_type(_: NodeID<Self>, _: &Ast) -> Result<NodeType> {
        Ok(NodeType::Void)
    }

    fn children(&self) -> NodeIterator<'_> {
        NodeIterator::single(self.value)
    }

    fn link(node_id: NodeID<Self>, ast: &mut Ast) -> Result<()> {
        let node: &Self = ast.get_inner(node_id);
        if let State::Unlinked(variable_name) = &node.variable {
            let variable_id = ast
                .closest_variable(node_id, variable_name)?
                .ok_or(Error::VariableNotFound)?;

            let body: &mut Self = ast.get_inner_mut(node_id);
            body.variable = State::Linked(variable_id);

            let variable = ast.get_typed(variable_id);
            if let Some(type_id) = variable.type_declaration_id() {
                let body = ast.get_inner_mut(node_id);
                body.is_static = true;

                let body = ast.get_inner(node_id);
                let path = body.field.as_ref().ok_or(Error::InternalError)?.clone();
                let value = body.value;

                let tp = ast.get_inner_mut(type_id);
                tp.associated_values.insert(path, value);
            }
        }
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

impl Node for VariableAssignment {
    fn node_type(_: NodeID<Self>, _: &Ast) -> Result<NodeType> {
        Ok(NodeType::Void)
    }

    fn children(&self) -> NodeIterator<'_> {
        NodeIterator::single(self.value)
    }

    fn link(node_id: NodeID<Self>, ast: &mut Ast) -> Result<()> {
        let node: &Self = ast.get_inner(node_id);
        if let State::Unlinked(var) = &node.variable {
            let var = ast
                .closest_variable(node_id, var)?
                .ok_or(Error::VariableNotFound)?;

            let node: &mut Self = ast.get_inner_mut(node_id);
            node.variable = State::Linked(var);
        }
        Ok(())
    }
}
