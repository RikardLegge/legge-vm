use crate::ast::AstContext;
use crate::node::{NodeIterator, NodeUsage};
use crate::{Ast, Block, Error, Expression, Node, NodeID, NodeType};

#[derive(Debug, Clone)]
pub struct If {
    pub cond: NodeID<Expression>,
    pub body: NodeID<Block>,
    pub r#else: Option<NodeID<Expression>>,
}

impl Node for If {
    fn children(&self, _context: AstContext) -> NodeIterator<'_> {
        let elements = NodeIterator::dual(self.cond, self.body);
        match self.r#else {
            Some(r#else) => NodeIterator::chained(elements, NodeIterator::single(r#else)),
            None => elements,
        }
    }

    fn check(node_id: NodeID<Self>, ast: &mut Ast) -> crate::Result<()> {
        let body = ast.get_body(node_id);
        let tp = ast.get_node_type(body.cond, NodeUsage::Value)?;
        if tp == NodeType::Boolean {
            Ok(())
        } else {
            Err(Error::TypeMissmatch(node_id.into(), body.cond.into()))
        }
    }
}

#[derive(Debug, Clone)]
pub struct Loop {
    pub body: NodeID<Block>,
}

impl Node for Loop {
    fn children(&self, _context: AstContext) -> NodeIterator<'_> {
        NodeIterator::single(self.body)
    }
}
