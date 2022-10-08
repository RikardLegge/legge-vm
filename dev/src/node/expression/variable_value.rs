use crate::ast::{NodeBody, NodeID, NodeUsage};
use crate::node::{
    closest_variable, get_node_type, Ast, AstContext, AstRootNode, NodeType, Variable,
};
use crate::{Error, State};

#[derive(Debug, Clone)]
pub struct VariableValue {
    pub variable: State<String, NodeID<Variable>>,
}

impl VariableValue {
    pub fn new(variable: State<String, NodeID<Variable>>) -> Self {
        Self { variable }
    }
}

impl NodeBody for VariableValue {
    type Root = AstRootNode;
    type NodeType = NodeType;
    type AstContext = AstContext;
    type Variable = Variable;

    fn node_type(
        node_id: NodeID<Self>,
        ast: &Ast,
        node_usage: NodeUsage,
    ) -> crate::Result<NodeType> {
        let value: &Self = ast.get_body(node_id);
        match value.variable {
            State::Linked(var) => get_node_type(ast, var, node_usage),
            _ => Err(Error::UnlinkedNode(node_id.into())),
        }
    }

    fn link(node_id: NodeID<Self>, ast: &mut Ast, context: AstContext) -> crate::Result<()> {
        let body: &Self = ast.get_body(node_id);
        if let State::Unlinked(var) = &body.variable {
            let var = closest_variable(ast, node_id, var, context)?
                .ok_or_else(|| panic!("variable not found {}", var))?; //Error::VariableNotFound(var.into()))?;

            let body: &mut Self = ast.get_body_mut(node_id);
            body.variable = State::Linked(var);
        }
        Ok(())
    }
}
