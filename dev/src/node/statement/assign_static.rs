use crate::ast::{NodeBody, NodeID, NodeIterator, NodeUsage};
use crate::node::{
    closest_variable, get_node_type, Ast, AstContext, AstNode, AstRootNode, Expression, NodeType,
    Variable, VariableValue,
};
use crate::{Error, State};

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

impl NodeBody for StaticAssignment {
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
        NodeIterator::chained(
            NodeIterator::single(self.assign_to),
            NodeIterator::dual(self.variable, self.value),
        )
    }

    fn link(node_id: NodeID<Self>, ast: &mut Ast, context: AstContext) -> crate::Result<()> {
        let node: &Self = ast.get_body(node_id);
        let assign_to = ast.get_body(node.assign_to);
        if let State::Unlinked(variable_name) = &assign_to.variable {
            let variable_id = closest_variable(ast, node_id, variable_name, context)?
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
