use crate::ast::{NodeBody, NodeID, NodeIterator, NodeUsage};
use crate::node::{
    get_node_type, Ast, AstContext, AstNode, AstRootNode, Expression, FunctionDeclaration,
    NodeType, Variable,
};
use crate::{Error, State};

#[derive(Debug, Clone)]
pub struct Return {
    pub func: State<(), NodeID<FunctionDeclaration>>,
    pub value: Option<NodeID<Expression>>,
}

impl NodeBody for Return {
    type Root = AstRootNode;
    type NodeType = NodeType;
    type AstContext = AstContext;
    type Variable = Variable;

    fn children(&self, _context: AstContext) -> NodeIterator<'_, Self::AstContext> {
        match self.value {
            Some(value) => NodeIterator::single(value),
            None => NodeIterator::empty(),
        }
    }

    fn link(node_id: NodeID<Self>, ast: &mut Ast, _context: AstContext) -> crate::Result<()> {
        let func = ast.walk_up(
            node_id,
            |node| match <&AstNode<FunctionDeclaration>>::try_from(node) {
                Ok(node) => Ok(Some(node.id)),
                Err(_) => Ok(None),
            },
        )?;
        if let Some(func) = func {
            ast.get_body_mut(node_id).func = State::Linked(func);
            Ok(())
        } else {
            panic!();
        }
    }

    fn check(node_id: NodeID<Self>, ast: &mut Ast) -> crate::Result<()> {
        let body = ast.get_body(node_id);
        let func: NodeID<FunctionDeclaration> = (&body.func)
            .try_into()
            .map_err(|_| Error::UnlinkedNode(node_id.into()))?;

        let expected_return = &ast.get_body(func).returns;
        let got_return = body
            .value
            .map(|id| get_node_type(ast, id, NodeUsage::Value))
            .transpose()?
            .unwrap_or(NodeType::Void);

        if &got_return == expected_return {
            Ok(())
        } else {
            let return_stmt_id = body
                .value
                .map(|id| id.into())
                .unwrap_or_else(|| node_id.into());
            Err(Error::TypeMissmatch(func.into(), return_stmt_id))
        }
    }
}
