use crate::ast::AstContext;
use crate::node::{
    Expression, Node, NodeID, NodeIterator, NodeType, NodeUsage, Return, Variable, VariableValue,
};
use crate::{Ast, AstNode, Block, Error, Result, State, Statement};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct FunctionDeclaration {
    pub arguments: Vec<NodeID<Variable>>,
    pub returns: NodeType,
    pub body: NodeID<Block>,
}

impl Node for FunctionDeclaration {
    fn node_type(node_id: NodeID<Self>, ast: &Ast, node_usage: NodeUsage) -> Result<NodeType> {
        match node_usage {
            NodeUsage::Type | NodeUsage::Value => Ok(NodeType::Function(node_id)),
            NodeUsage::Call => {
                let body = ast.get_body(node_id);
                Ok(body.returns.clone())
            }
        }
    }

    fn children(&self, _context: AstContext) -> NodeIterator<'_> {
        NodeIterator::chained(
            NodeIterator::slice(&self.arguments),
            NodeIterator::single(self.body),
        )
    }

    fn check(node_id: NodeID<Self>, ast: &mut Ast) -> Result<()> {
        let node = ast.get_body(node_id);
        let body = ast.get_body(node.body);

        let last = body.children.last();

        match last {
            None if node.returns == NodeType::Void => Ok(()),
            None => Err(Error::TypeMissmatch(node_id.into(), node.body.into())),
            Some(last_id) => {
                let last = ast.get(*last_id);
                let last: Result<&AstNode<Return>, Error> = last.try_into();
                match last {
                    Ok(_) => {
                        // We don't have to check the value of the return here since the return will
                        // be checked separately. Since we know that this is a return statement, we
                        // should be ok!
                        Ok(())
                    }
                    Err(_) if node.returns == NodeType::Void => Ok(()),
                    Err(_) => Err(Error::TypeMissmatch(node_id.into(), (*last_id).into())),
                }
            }
        }
    }
}

impl Statement {
    pub fn variable(&self) -> Option<NodeID<Variable>> {
        match self {
            Statement::VariableDeclaration(dec) => Some(dec.variable),
            Statement::VariableAssignment(_) => None,
            Statement::TypeDeclaration(dec) => Some(dec.variable),
            Statement::StaticAssignment(_) => None,
            Statement::EvaluateExpression(_) => None,
            Statement::Return(_) => None,
            Statement::Break(_) => None,
        }
    }

    pub fn value(&self) -> Option<NodeID> {
        match self {
            Statement::VariableDeclaration(var) => Some(var.value.into()),
            Statement::VariableAssignment(var) => Some(var.value.into()),
            Statement::TypeDeclaration(var) => Some(var.constructor.into()),
            Statement::StaticAssignment(var) => Some(var.value.into()),
            Statement::EvaluateExpression(var) => Some(var.value.into()),
            Statement::Return(var) => var.value.map(Into::into),
            Statement::Break(var) => var.value.map(Into::into),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypeDeclaration {
    variable: NodeID<Variable>,
    constructor: NodeID<Expression>,
    fields: Vec<NodeID>,
    associated_values: HashMap<String, NodeID<Variable>>,
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

impl Node for TypeDeclaration {
    fn node_type(node_id: NodeID<Self>, _: &Ast, node_usage: NodeUsage) -> Result<NodeType> {
        match node_usage {
            NodeUsage::Type => Ok(NodeType::Custom(node_id)),
            NodeUsage::Call | NodeUsage::Value => Ok(NodeType::Void),
        }
    }

    fn children(&self, _context: AstContext) -> NodeIterator<'_> {
        let props = NodeIterator::dual(self.variable, self.constructor);
        let fields = NodeIterator::slice(&self.fields);
        NodeIterator::chained(props, fields)
    }

    fn has_variable(&self, var: &str) -> Result<Option<NodeID<Variable>>> {
        if let Some(variable_id) = self.associated_values.get(var) {
            Ok(Some(*variable_id))
        } else {
            Ok(None)
        }
    }
}

#[derive(Debug, Clone)]
pub struct VariableDeclaration {
    variable: NodeID<Variable>,
    value: NodeID<Expression>,
    is_const: bool,
}

impl VariableDeclaration {
    pub fn new(variable: NodeID<Variable>, value: NodeID<Expression>, is_const: bool) -> Self {
        VariableDeclaration {
            variable,
            value,
            is_const,
        }
    }
}

impl Node for VariableDeclaration {
    fn node_type(node_id: NodeID<Self>, ast: &Ast, node_usage: NodeUsage) -> Result<NodeType> {
        match node_usage {
            NodeUsage::Type => {
                let body = ast.get_body(node_id);
                ast.get_node_type(body.value, node_usage)
            }
            NodeUsage::Call | NodeUsage::Value => Ok(NodeType::Void),
        }
    }

    fn children(&self, _context: AstContext) -> NodeIterator<'_> {
        NodeIterator::dual(self.variable, self.value)
    }
}

#[derive(Debug, Clone)]
pub struct StaticAssignment {
    pub assign_to: NodeID<VariableValue>,
    pub variable: NodeID<Variable>,
    pub value: NodeID<Expression>,
    pub is_associated_field: bool,
}

impl AstNode<StaticAssignment> {
    pub fn parent_id(&self) -> NodeID {
        self.parent_id.unwrap()
    }
}

impl StaticAssignment {
    pub fn new(
        assign_to: NodeID<VariableValue>,
        variable: NodeID<Variable>,
        value: NodeID<Expression>,
    ) -> Self {
        StaticAssignment {
            assign_to,
            variable,
            value,
            is_associated_field: false,
        }
    }
}

impl Node for StaticAssignment {
    fn node_type(node_id: NodeID<Self>, ast: &Ast, node_usage: NodeUsage) -> Result<NodeType> {
        match node_usage {
            NodeUsage::Type => {
                let body = ast.get_body(node_id);
                ast.get_node_type(body.value, node_usage)
            }
            NodeUsage::Call | NodeUsage::Value => Ok(NodeType::Void),
        }
    }

    fn children(&self, _context: AstContext) -> NodeIterator<'_> {
        NodeIterator::chained(
            NodeIterator::single(self.assign_to),
            NodeIterator::dual(self.variable, self.value),
        )
    }

    fn link(node_id: NodeID<Self>, ast: &mut Ast, context: AstContext) -> Result<()> {
        let node: &Self = ast.get_body(node_id);
        let assign_to = ast.get_body(node.assign_to);
        if let State::Unlinked(variable_name) = &assign_to.variable {
            let variable_id = ast
                .closest_variable(node_id, variable_name, context)?
                .ok_or_else(|| Error::VariableNotFound(variable_name.into()))?;

            let body = ast.get_body_mut(node.assign_to);
            body.variable = State::Linked(variable_id);

            if let Some(type_id) = AstNode::type_declaration_id(variable_id, ast) {
                let body = ast.get_body_mut(node_id);
                body.is_associated_field = true;

                let body = ast.get_body(node_id);
                let variable = body.variable;
                let path = ast.get_body(variable).name.clone();

                let tp = ast.get_body_mut(type_id);
                tp.associated_values.insert(path, variable);
            }
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
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
    fn node_type(_: NodeID<Self>, _: &Ast, _node_usage: NodeUsage) -> Result<NodeType> {
        Ok(NodeType::Void)
    }

    fn children(&self, _context: AstContext) -> NodeIterator<'_> {
        NodeIterator::single(self.value)
    }

    fn link(node_id: NodeID<Self>, ast: &mut Ast, context: AstContext) -> Result<()> {
        let node: &Self = ast.get_body(node_id);
        if let State::Unlinked(var) = &node.variable {
            let var = ast
                .closest_variable(node_id, var, context)?
                .ok_or_else(|| Error::VariableNotFound(var.into()))?;

            let node: &mut Self = ast.get_body_mut(node_id);
            node.variable = State::Linked(var);
        }
        Ok(())
    }

    fn check(node_id: NodeID<Self>, ast: &mut Ast) -> Result<()> {
        let node = ast.get_body(node_id);

        let variable_id: NodeID<Variable> = (&node.variable)
            .try_into()
            .map_err(|_| Error::UnlinkedNode(node_id.into()))?;
        let lhs_tp = ast.get_node_type(variable_id, NodeUsage::Type)?;

        let value_id = node.value;
        let rhs_tp = ast.get_node_type(value_id, NodeUsage::Value)?;

        if lhs_tp == rhs_tp {
            Ok(())
        } else {
            Err(Error::TypeMissmatch(variable_id.into(), value_id.into()))
        }
    }
}

#[derive(Debug, Clone)]
pub struct EvaluateExpression {
    pub value: NodeID<Expression>,
    pub has_end_statement: bool,
}

impl Node for EvaluateExpression {
    fn node_type(node_id: NodeID<Self>, ast: &Ast, usage: NodeUsage) -> Result<NodeType> {
        let node = ast.get_body(node_id);
        ast.get_node_type(node.value, usage)
    }

    fn children(&self, _context: AstContext) -> NodeIterator<'_> {
        NodeIterator::single(self.value)
    }
}

impl EvaluateExpression {
    pub fn new(value: impl Into<NodeID<Expression>>) -> Self {
        EvaluateExpression {
            value: value.into(),
            has_end_statement: true,
        }
    }
}
