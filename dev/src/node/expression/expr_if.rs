use crate::ast::{NodeBody, NodeID, NodeIterator, NodeUsage};
use crate::node::{
    get_node_type, Ast, AstContext, AstRootNode, Block, Expression, NodeType, Variable,
};
// use crate::{Error, Result};
//
// #[derive(Debug, Clone)]
// pub struct If {
//     pub cond: NodeID<Expression>,
//     pub body: NodeID<Block>,
//     pub r#else: Option<NodeID<Expression>>,
// }

impl NodeBody for If {
    type Root = AstRootNode;
    type NodeType = NodeType;
    type AstContext = AstContext;
    type Variable = Variable;

    // fn node_type(node_id: NodeID<Self>, ast: &Ast, usage: NodeUsage) -> Result<NodeType> {
    //     match usage {
    //         NodeUsage::Type => Err(Error::InternalError),
    //         NodeUsage::Call => Err(Error::InternalError),
    //         NodeUsage::Value => {
    //             let node = ast.get_body(node_id);
    //             get_node_type(ast, node.body, usage)
    //         }
    //     }
    // }
    //
    // fn children(&self, _context: AstContext) -> NodeIterator<'_, Self::AstContext> {
    //     let elements = NodeIterator::dual(self.cond, self.body);
    //     match self.r#else {
    //         Some(r#else) => NodeIterator::chained(elements, NodeIterator::single(r#else)),
    //         None => elements,
    //     }
    // }

    fn check(node_id: NodeID<Self>, ast: &mut Ast) -> Result<()> {
        let body = ast.get_body(node_id);
        let cond_tp = get_node_type(ast, body.cond, NodeUsage::Value)?;
        if cond_tp != NodeType::Boolean {
            return Err(Error::TypeMissmatch(node_id.into(), body.cond.into()));
        }

        if let Some(r#else) = body.r#else {
            let body_tp = get_node_type(ast, body.body, NodeUsage::Value)?;
            let else_tp = get_node_type(ast, r#else, NodeUsage::Value)?;
            if body_tp != else_tp {
                return Err(Error::TypeMissmatch(body.body.into(), r#else.into()));
            }
        }

        Ok(())
    }
}
