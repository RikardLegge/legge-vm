use crate::ast::{AstNode, AstNodeRef};
use crate::children::{ChildIterator, Children};
use crate::linker::Linker;
use crate::node::{
    Ast, Error, NodeID, Result, Statement, TypeDeclaration, Variable, VariableDeclaration,
};
use crate::types::{NodeType, NodeUsage, Types};
use std::borrow::Cow;

#[derive(Debug)]
pub struct VariableStorage {
    pub name: String,
    pub tp: Option<NodeType>,
}

impl VariableStorage {
    pub fn new(name: String) -> Self {
        VariableStorage { name, tp: None }
    }
}

impl Types for AstNodeRef<Variable> {
    fn get_type<'this, 'ast>(
        &'this self,
        ast: &'ast Ast,
        usage: NodeUsage,
    ) -> Result<Cow<'ast, NodeType>> {
        if let Some(tp) = &ast.body(self.id).tp {
            return Ok(Cow::Borrowed(tp));
        }

        let parent_id = self.parent_id.ok_or_else(|| {
            let err = Error::ExpectedParent(self.id.into());
            panic!("{:?}", err);
            err
        })?;
        let parent = ast.get(parent_id);

        let statement: &AstNode<Statement> = parent.try_into()?;
        match usage {
            NodeUsage::Type => ast.get(parent_id).get_type(ast, usage),
            NodeUsage::Value => {
                let value_id = statement.value().ok_or_else(|| {
                    let err = Error::ExpectedValue(statement.id.into());
                    panic!("{:?}", err);
                    err
                })?;
                ast.get(value_id).get_type(ast, usage)
            }
        }
    }
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

    pub fn variable_declaration_id(
        node_id: NodeID<Variable>,
        ast: &Ast,
    ) -> Option<NodeID<VariableDeclaration>> {
        let node = ast.get(node_id);
        let parent = ast.get(node.parent_id?);
        let declaration: &AstNode<VariableDeclaration> = parent.try_into().ok()?;
        Some(declaration.id)
    }
}

impl Children for AstNodeRef<Variable> {
    fn children<'this, 'ast>(&'this self, _ast: &'ast Ast) -> ChildIterator<'ast> {
        ChildIterator::new([].into())
    }
}

impl Linker for AstNodeRef<Variable> {}
