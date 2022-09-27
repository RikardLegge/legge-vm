use crate::node::{AstNodeBody, NodeType, TypeDeclaration};
use crate::{Ast, AstNode, Error, Result};
use crate::{Node, NodeID};

#[derive(Debug)]
pub enum ReferenceType {
    VariableDeclaration,
    TypeDeclaration,
}

#[derive(Debug)]
pub struct Variable {
    pub name: String,
    reference_type: ReferenceType,
}

impl AstNode<Variable> {
    pub fn type_declaration_id(&self) -> Option<NodeID<TypeDeclaration>> {
        match self.body().reference_type {
            ReferenceType::TypeDeclaration => self
                .parent_id
                .map(|parent_id| unsafe { std::mem::transmute(parent_id) }),
            ReferenceType::VariableDeclaration => None,
        }
    }
}

impl Variable {
    pub fn new(name: String, reference_type: ReferenceType) -> Self {
        Variable {
            name,
            reference_type,
        }
    }
}

impl Node for Variable {
    fn node_type(node_id: NodeID<Self>, ast: &Ast) -> Result<NodeType> {
        let node = ast.get(node_id);
        let parent_id = node.parent_id.ok_or(Error::InternalError)?;
        let parent = ast.get(parent_id);
        match parent.body.as_ref().unwrap() {
            AstNodeBody::Statement(statement) => {
                let value = statement.value().ok_or(Error::InternalError)?;
                ast.get_node_type(value)
            }
            _ => unimplemented!(),
        }
    }
}
