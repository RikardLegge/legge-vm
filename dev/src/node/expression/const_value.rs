use crate::ast::{NodeBody, NodeID, NodeUsage};
use crate::node::{Ast, AstContext, AstRootNode, NodeType, TypeDeclaration, Variable};

#[derive(Debug, Clone)]
pub enum Value {
    Int(isize),
    Float(f64),
    String(String),
    Custom(NodeID<TypeDeclaration>),
}

impl NodeBody for Value {
    type Root = AstRootNode;
    type NodeType = NodeType;
    type AstContext = AstContext;
    type Variable = Variable;

    fn node_type(
        node_id: NodeID<Self>,
        ast: &Ast,
        _node_usage: NodeUsage,
    ) -> crate::Result<NodeType> {
        let value: &Value = ast.get_body(node_id);
        Ok(match value {
            Value::Int(_) => NodeType::Int,
            Value::Float(_) => NodeType::Float,
            Value::String(_) => NodeType::String,
            Value::Custom(id) => NodeType::Custom(*id),
        })
    }
}
