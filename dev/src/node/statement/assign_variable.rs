use crate::ast::{NodeBody, NodeID, NodeIterator, NodeUsage};
use crate::node::{
    closest_variable, get_node_type, Ast, AstContext, AstRootNode, Expression, NodeType, Variable,
};
use crate::{Error, State};

#[derive(Debug, Clone)]
pub struct VariableAssignment {
    pub variable: State<String, NodeID<Variable>>,
    pub value: NodeID<Expression>,
}

impl VariableAssignment {
    pub fn new(ident: String, value: NodeID<Expression>) -> Self {
        VariableAssignment {
            variable: State::Unlinked(ident),
            value,
        }
    }
}

impl NodeBody for VariableAssignment {
    type Root = AstRootNode;
    type NodeType = NodeType;
    type AstContext = AstContext;
    type Variable = Variable;

    fn node_type(_: NodeID<Self>, _: &Ast, _node_usage: NodeUsage) -> crate::Result<NodeType> {
        Ok(NodeType::Void)
    }

    fn children(&self, _context: AstContext) -> NodeIterator<'_, Self::AstContext> {
        NodeIterator::single(self.value)
    }

    fn link(node_id: NodeID<Self>, ast: &mut Ast, context: AstContext) -> crate::Result<()> {
        let node: &Self = ast.get_body(node_id);
        if let State::Unlinked(var) = &node.variable {
            let var = closest_variable(ast, node_id, var, context)?
                .ok_or_else(|| Error::VariableNotFound(var.into()))?;

            let node: &mut Self = ast.get_body_mut(node_id);
            node.variable = State::Linked(var);
        }
        Ok(())
    }

    fn check(node_id: NodeID<Self>, ast: &mut Ast) -> crate::Result<()> {
        let node = ast.get_body(node_id);

        let variable_id: NodeID<Variable> = (&node.variable)
            .try_into()
            .map_err(|_| Error::UnlinkedNode(node_id.into()))?;
        let lhs_tp = get_node_type(ast, variable_id, NodeUsage::Type)?;

        let value_id = node.value;
        let rhs_tp = get_node_type(ast, value_id, NodeUsage::Value)?;

        if lhs_tp == rhs_tp {
            Ok(())
        } else {
            Err(Error::TypeMissmatch(variable_id.into(), value_id.into()))
        }
    }
}
