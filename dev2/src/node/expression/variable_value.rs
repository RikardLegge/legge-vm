use crate::ast::{AstNode, AstNodeRef};
use crate::children::{ChildIterator, Children};
use crate::node::statement::ReturnStorage;
use crate::node::{
    Ast, Block, Expression, Loop, NodeID, Result, Statement, TypeDeclaration, Variable,
    VariableValue,
};
use crate::state::State;
use crate::types::{NodeType, NodeUsage, Types};
use std::borrow::Cow;
use std::collections::HashMap;
use std::ops::Deref;

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
            _ => panic!(), //Err(Error::UnlinkedNode(node_id.into())),
        }
    }
}

impl Children for AstNodeRef<VariableValue> {
    fn children<'this, 'ast>(&'this self, _ast: &'ast Ast) -> ChildIterator<'ast> {
        ChildIterator::new([].into())
    }
}
