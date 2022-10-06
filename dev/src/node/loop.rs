use crate::ast::AstContext;
use crate::node::{NodeIterator, NodeUsage};
use crate::{Ast, Block, Error, Expression, Node, NodeID, NodeType, Variable};

#[derive(Debug, Clone)]
pub struct If {
    pub cond: NodeID<Expression>,
    pub body: NodeID<Block>,
    pub r#else: Option<NodeID<Expression>>,
}

impl Node for If {
    fn node_type(node_id: NodeID<Self>, ast: &Ast, usage: NodeUsage) -> crate::Result<NodeType> {
        match usage {
            NodeUsage::Type => Err(Error::InternalError),
            NodeUsage::Call => Err(Error::InternalError),
            NodeUsage::Value => {
                let node = ast.get_body(node_id);
                ast.get_node_type(node.body, usage)
            }
        }
    }

    fn children(&self, _context: AstContext) -> NodeIterator<'_> {
        let elements = NodeIterator::dual(self.cond, self.body);
        match self.r#else {
            Some(r#else) => NodeIterator::chained(elements, NodeIterator::single(r#else)),
            None => elements,
        }
    }

    fn check(node_id: NodeID<Self>, ast: &mut Ast) -> crate::Result<()> {
        let body = ast.get_body(node_id);
        let cond_tp = ast.get_node_type(body.cond, NodeUsage::Value)?;
        if cond_tp != NodeType::Boolean {
            return Err(Error::TypeMissmatch(node_id.into(), body.cond.into()));
        }

        if let Some(r#else) = body.r#else {
            let body_tp = ast.get_node_type(body.body, NodeUsage::Value)?;
            let else_tp = ast.get_node_type(r#else, NodeUsage::Value)?;
            if body_tp != else_tp {
                return Err(Error::TypeMissmatch(body.body.into(), r#else.into()));
            }
        }

        Ok(())
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
