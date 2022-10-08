use crate::ast::{NodeBody, NodeID, NodeIterator, NodeUsage};
use crate::node::{get_node_type, Ast, AstContext, AstRootNode, Expression, NodeType, Variable};
use crate::token::ArithmeticOP;
use crate::Error;

#[derive(Debug, Clone)]
pub struct Operation {
    pub op: ArithmeticOP,
    pub lhs: NodeID<Expression>,
    pub rhs: NodeID<Expression>,
    pub tp: Option<NodeType>,
}

impl Operation {
    pub fn new(op: ArithmeticOP, lhs: NodeID<Expression>, rhs: NodeID<Expression>) -> Self {
        Operation {
            op,
            lhs,
            rhs,
            tp: None,
        }
    }
}

impl NodeBody for Operation {
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
            NodeUsage::Value => {
                let op: &Self = ast.get_body(node_id);
                let lhs_type = get_node_type(ast, op.lhs, NodeUsage::Value)?;
                let rhs_type = get_node_type(ast, op.lhs, NodeUsage::Value)?;
                if lhs_type == rhs_type {
                    match op.op {
                        ArithmeticOP::Add
                        | ArithmeticOP::Sub
                        | ArithmeticOP::Mul
                        | ArithmeticOP::Div => Ok(lhs_type),
                        ArithmeticOP::Eq | ArithmeticOP::GEq | ArithmeticOP::LEq => {
                            Ok(NodeType::Boolean)
                        }
                    }
                } else {
                    Err(Error::TypeMissmatch(op.lhs.into(), op.rhs.into()))
                }
            }
            _ => Err(Error::InternalError),
        }
    }

    fn children(&self, _context: AstContext) -> NodeIterator<'_, Self::AstContext> {
        NodeIterator::dual(self.lhs, self.rhs)
    }

    fn link(_: NodeID<Self>, _: &mut Ast, _context: AstContext) -> crate::Result<()> {
        Ok(())
    }
}
