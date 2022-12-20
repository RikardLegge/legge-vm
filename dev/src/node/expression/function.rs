use crate::ast::{NodeBody, NodeID, NodeIterator, NodeUsage};
use crate::node::{Ast, AstContext, AstNode, AstRootNode, Block, NodeType, Return, Variable};
use crate::Error;

#[derive(Debug, Clone)]
pub struct FunctionDeclaration {
    pub arguments: Vec<NodeID<Variable>>,
    pub returns: NodeType,
    pub body: NodeID<Block>,
}

impl NodeBody for FunctionDeclaration {
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
            NodeUsage::Type | NodeUsage::Value => Ok(NodeType::Function(node_id)),
            NodeUsage::Call => {
                let body = ast.get_body(node_id);
                Ok(body.returns.clone())
            }
        }
    }

    fn children(&self, _context: AstContext) -> NodeIterator<'_, Self::AstContext> {
        NodeIterator::chained(
            NodeIterator::slice(&self.arguments),
            NodeIterator::single(self.body),
        )
    }

    fn check(node_id: NodeID<Self>, ast: &mut Ast) -> crate::Result<()> {
        let node = ast.get_body(node_id);
        let body = ast.get_body(node.body);

        let last = body.children.last();

        match last {
            None if node.returns == NodeType::Void => Ok(()),
            None => Err(Error::TypeMissmatch(node_id.into(), node.body.into())),
            Some(last_id) => {
                let last = ast.get(*last_id);
                let last: crate::Result<&AstNode<Return>, Error> = last.try_into();
                match last {
                    Ok(_) => {
                        // We don't have to check the value of the return here since the return will
                        // be checked separately. Since we know that this is a return statement, we
                        // should be ok!
                        Ok(())
                    }
                    Err(_) if node.returns == NodeType::Void => Ok(()),
                    Err(_) => Err(Error::TypeMissmatch(node_id.into(), (*last_id).into())),
                }
            }
        }
    }
}
