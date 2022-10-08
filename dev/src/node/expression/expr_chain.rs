use crate::ast::{NodeBody, NodeID, NodeIDContext, NodeIterator, NodeUsage};
use crate::node::{
    get_node_type, Ast, AstContext, AstNode, AstRootNode, Expression, FunctionCall, NodeType,
    Variable, VariableValue,
};
use crate::{Error, Result, State};

#[derive(Copy, Clone, Debug)]
pub struct ExpressionChain {
    pub lhs: NodeID<Expression>,
    pub rhs: NodeID<Expression>,
    linked: bool,
}

impl ExpressionChain {
    pub fn new(lhs: NodeID<Expression>, rhs: NodeID<Expression>) -> Self {
        Self {
            lhs,
            rhs,
            linked: false,
        }
    }
}

impl NodeBody for ExpressionChain {
    type Root = AstRootNode;
    type NodeType = NodeType;
    type AstContext = AstContext;
    type Variable = Variable;

    fn node_type(node_id: NodeID<Self>, ast: &Ast, usage: NodeUsage) -> Result<NodeType> {
        let body = ast.get_body(node_id);
        get_node_type(ast, body.rhs, usage)
    }

    fn children(&self, _context: AstContext) -> NodeIterator<'_, Self::AstContext> {
        NodeIterator::dual(
            self.lhs,
            NodeIDContext {
                node_id: self.rhs.into(),
                context: AstContext::Chain(self.lhs.into()),
            },
        )
    }

    fn link(node_id: NodeID<Self>, ast: &mut Ast, _context: AstContext) -> Result<()> {
        let node = ast.get_body(node_id);

        if node.linked {
            return Ok(());
        }
        let lhs_id = node.lhs;
        let lhs = ast.get_body(lhs_id);
        let lhs_variable = match lhs {
            Expression::VariableValue(value) => {
                let variable_id: NodeID<Variable> = (&value.variable)
                    .try_into()
                    .map_err(|_| Error::UnlinkedNode(lhs_id.into()))?;

                <AstNode<Variable>>::variable_declaration_id(variable_id, ast).map(|_| variable_id)
            }
            _ => None,
        };

        // If the lhs of the expression is a variable, not a type for example,
        // then we want to insert the variable as an implicit first parameter in the call.
        if let Some(variable_id) = lhs_variable {
            let rhs = ast.get(node.rhs);
            if let Ok(rhs) = <&AstNode<FunctionCall>>::try_from(rhs) {
                let value =
                    ast.push_new_node(node.rhs, VariableValue::new(State::Linked(variable_id)));

                let call = ast.get_body_mut(rhs.id);
                call.args.insert(0, value.into());
            }
        }

        ast.get_body_mut(node_id).linked = true;
        Ok(())
    }
}
