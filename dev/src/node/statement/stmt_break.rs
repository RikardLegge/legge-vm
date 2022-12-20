use crate::ast::{NodeBody, NodeID, NodeIterator, NodeUsage};
use crate::node::{
    get_node_type, Ast, AstContext, AstNode, AstRootNode, Expression, Loop, NodeType, Variable,
};
use crate::{Error, State};

// #[derive(Debug, Clone)]
// pub struct Break {
//     pub r#loop: State<(), NodeID<Loop>>,
//     pub value: Option<NodeID<Expression>>,
// }
//
// impl NodeBody for Break {
//     type Root = AstRootNode;
//     type NodeType = NodeType;
//     type AstContext = AstContext;
//     type Variable = Variable;
//
//     fn children(&self, _context: AstContext) -> NodeIterator<'_, Self::AstContext> {
//         match self.value {
//             Some(value) => NodeIterator::single(value),
//             None => NodeIterator::empty(),
//         }
//     }

    fn link(node_id: NodeID<Self>, ast: &mut Ast, _context: AstContext) -> crate::Result<()> {
        let node = ast.get_body(node_id);
        let loop_id = match node.r#loop {
            State::Unlinked(_) => {
                let loop_id =
                    ast.walk_up(node_id, |node| match <&AstNode<Loop>>::try_from(node) {
                        Ok(node) => Ok(Some(node.id)),
                        Err(_) => Ok(None),
                    })?;
                if let Some(loop_id) = loop_id {
                    ast.get_body_mut(node_id).r#loop = State::Linked(loop_id);
                    loop_id
                } else {
                    unimplemented!();
                }
            }
            State::Linked(id) => id,
        };

        let loop_node = ast.get_body(loop_id);
        if loop_node.value.is_none() {
            let node = ast.get_body(node_id);
            let tp = if let Some(value) = node.value {
                get_node_type(ast, value, NodeUsage::Value)?
            } else {
                NodeType::Void
            };
            let loop_node = ast.get_body_mut(loop_id);
            loop_node.value = Some(tp);
        };

        Ok(())
    }

    fn check(node_id: NodeID<Self>, ast: &mut Ast) -> crate::Result<()> {
        let node = ast.get_body(node_id);
        let tp = if let Some(value) = node.value {
            get_node_type(ast, value, NodeUsage::Value)?
        } else {
            NodeType::Void
        };
        let loop_id: NodeID<Loop> = (&node.r#loop)
            .try_into()
            .map_err(|_| Error::UnlinkedNode(node_id.into()))?;
        let loop_tp = ast
            .get_body(loop_id)
            .value
            .as_ref()
            .ok_or_else(|| Error::TypeNotInferred(loop_id.into()))?;

        if &tp == loop_tp {
            Ok(())
        } else {
            Err(Error::TypeMissmatch(node_id.into(), loop_id.into()))
        }
    }
}
