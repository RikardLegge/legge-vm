use crate::ast::{NodeBody, NodeID, NodeIterator, NodeUsage};
use crate::node::{
    get_node_type, Ast, AstContext, AstRootNode, Expression, NodeType, Statement, Variable,
};

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

impl NodeBody for VariableDeclaration {
    type Root = AstRootNode;
    type NodeType = NodeType;
    type AstContext = AstContext;
    type Variable = Variable;

    fn node_type(
        node_id: NodeID<Self>,
        ast: &Ast,
        node_usage: NodeUsage,
    ) -> crate::Result<NodeType> {
        match node_usage {
            NodeUsage::Type => {
                let body = ast.get_body(node_id);
                get_node_type(ast, body.value, node_usage)
            }
            NodeUsage::Call | NodeUsage::Value => Ok(NodeType::Void),
        }
    }

    fn children(&self, _context: AstContext) -> NodeIterator<'_, Self::AstContext> {
        NodeIterator::dual(self.variable, self.value)
    }
}
