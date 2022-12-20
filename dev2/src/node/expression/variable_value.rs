use crate::ast::{AstNode, AstNodeRef};
use crate::children::{ChildIterator, Children};
use crate::linker::LinkContext::Node;
use crate::linker::{LinkContext, Linker, LinkerExt};
use crate::node::statement::ReturnStorage;
use crate::node::{
    Ast, Block, Error, Expression, Loop, NodeID, Result, Statement, Storage, TypeDeclaration,
    Variable, VariableValue,
};
use crate::state::State;
use crate::types::{NodeType, NodeUsage, Types};
use std::borrow::{Borrow, Cow};
use std::collections::HashMap;
use std::convert::Infallible;
use std::ops::Deref;
use std::os::unix::process::parent_id;

#[derive(Debug)]
pub struct VariableValueStorage {
    pub variable: State<String, NodeID<Variable>>,
}

impl VariableValueStorage {
    pub fn new(variable: State<String, NodeID<Variable>>) -> Self {
        Self { variable }
    }
}

impl Types for AstNodeRef<VariableValue> {
    fn get_type<'this, 'ast>(
        &'this self,
        ast: &'ast Ast,
        usage: NodeUsage,
    ) -> Result<Cow<'ast, NodeType>> {
        let node = ast.body(self.id);
        match node.variable {
            State::Linked(var) => ast.get(var).get_type(ast, usage),
            _ => Err(Error::UnlinkedNode(self.id.into())),
        }
    }
}

impl Children for AstNodeRef<VariableValue> {
    fn children<'this, 'ast>(&'this self, _ast: &'ast Ast) -> ChildIterator<'ast> {
        ChildIterator::new([].into())
    }
}

impl Linker for AstNodeRef<VariableValue> {
    fn link(&self, ast: &mut Ast, context: LinkContext) -> Result<()> {
        let body = ast.body(self.id);
        if let State::Unlinked(var) = &body.variable {
            let var = ast
                .closest_variable(self.id, var, context)?
                .ok_or_else(|| Error::VariableNotFound(var.into(), context))
                .map_err(|err| {
                    panic!("{:?}", err);
                    err
                })?;

            let body = ast.body_mut(self.id);
            body.variable = State::Linked(var);
        }
        Ok(())
    }
}
