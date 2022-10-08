use crate::ast::{NodeBody, NodeID, NodeIterator, NodeUsage};
use crate::node::{get_node_type, Ast, AstContext, AstRootNode, Expression, NodeType, Variable};

#[derive(Debug, Clone)]
pub struct EvaluateExpression {
    pub value: NodeID<Expression>,
    pub has_end_statement: bool,
}

impl NodeBody for EvaluateExpression {
    type Root = AstRootNode;
    type NodeType = NodeType;
    type AstContext = AstContext;
    type Variable = Variable;

    fn node_type(node_id: NodeID<Self>, ast: &Ast, usage: NodeUsage) -> crate::Result<NodeType> {
        let node = ast.get_body(node_id);
        get_node_type(ast, node.value, usage)
    }

    fn children(&self, _context: AstContext) -> NodeIterator<'_, Self::AstContext> {
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
