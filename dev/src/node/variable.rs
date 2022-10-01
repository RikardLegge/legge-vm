use crate::node::{NodeType, NodeUsage, TypeDeclaration};
use crate::{Ast, AstNode, Error, Result, Statement};
use crate::{Node, NodeID};

#[derive(Debug, Clone)]
pub struct Variable {
    pub name: String,
}

impl AstNode<Variable> {
    pub fn type_declaration_id(
        node_id: NodeID<Variable>,
        ast: &Ast,
    ) -> Option<NodeID<TypeDeclaration>> {
        let node = ast.get(node_id);
        let parent = ast.get(node.parent_id?);
        let declaration: &AstNode<TypeDeclaration> = parent.try_into().ok()?;
        Some(declaration.id)
    }
}

impl Variable {
    pub fn new(name: String) -> Self {
        Variable { name }
    }
}

impl Node for Variable {
    fn node_type(node_id: NodeID<Self>, ast: &Ast, node_usage: NodeUsage) -> Result<NodeType> {
        let node = ast.get(node_id);
        let parent_id = node.parent_id.ok_or(Error::InternalError)?;
        let parent = ast.get(parent_id);

        let statement: &Statement = parent.body.as_ref().unwrap().try_into()?;
        return match node_usage {
            NodeUsage::Type => ast.get_node_type(parent_id, node_usage),
            NodeUsage::Call | NodeUsage::Value => {
                let value = statement.value().ok_or(Error::InternalError)?;
                ast.get_node_type(value, node_usage)
            }
        };
    }
}
