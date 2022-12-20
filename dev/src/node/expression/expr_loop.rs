use crate::ast::{NodeBody, NodeID, NodeIterator, NodeUsage};
use crate::node::{Ast, AstContext, AstRootNode, Block, NodeType, Variable};
use crate::{Error, Result};

#[derive(Debug, Clone)]
pub struct Loop {
    pub body: NodeID<Block>,
    pub value: Option<NodeType>,
}

impl NodeBody for Loop {
    type Root = AstRootNode;
    type NodeType = NodeType;
    type AstContext = AstContext;
    type Variable = Variable;

    fn node_type(node_id: NodeID<Self>, ast: &Ast, _usage: NodeUsage) -> Result<NodeType> {
        let node = ast.get_body(node_id);
        if let Some(value) = &node.value {
            Ok(value.clone())
        } else {
            Err(Error::TypeNotInferred(node_id.into()))
        }
    }

    fn children(&self, _context: AstContext) -> NodeIterator<'_, Self::AstContext> {
        NodeIterator::single(self.body)
    }
}
