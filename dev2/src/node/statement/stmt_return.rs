use crate::ast::{AstNode, AstNodeRef};
use crate::children::{ChildIterator, Children};
use crate::linker::{LinkContext, Linker};
use crate::node::{
    Ast, Break, Error, Expression, FunctionDeclaration, Loop, NodeID, Result, Return,
    TypeDeclaration, Variable,
};
use crate::state::State;
use crate::types::{NodeType, NodeUsage, Types};
use std::borrow::Cow;

#[derive(Debug)]
pub struct ReturnStorage {
    pub func: State<(), NodeID<FunctionDeclaration>>,
    pub value: Option<NodeID<Expression>>,
}

impl ReturnStorage {
    pub fn new(value: Option<NodeID<Expression>>) -> Self {
        Self {
            func: ().into(),
            value,
        }
    }
}

impl Types for AstNodeRef<Return> {
    fn get_type<'this, 'ast>(
        &'this self,
        _ast: &'ast Ast,
        _usage: NodeUsage,
    ) -> Result<Cow<'ast, NodeType>> {
        Ok(Cow::Owned(NodeType::Void))
    }
}

impl Children for AstNodeRef<Return> {
    fn children<'this, 'ast>(&'this self, ast: &'ast Ast) -> ChildIterator<'ast> {
        let node = ast.body(self.id);
        match node.value {
            Some(value) => ChildIterator::new(([value.into()].into())),
            None => ChildIterator::new([].into()),
        }
    }
}

impl Linker for AstNodeRef<Return> {
    fn link(&self, ast: &mut Ast, context: LinkContext) -> Result<()> {
        let func = ast.walk_up(
            self.id,
            |node| match <&AstNode<FunctionDeclaration>>::try_from(node) {
                Ok(node) => Ok::<_, Error>(Some(node.id)),
                Err(_) => Ok(None),
            },
        )?;
        if let Some(func) = func {
            ast.body_mut(self.id).func = State::Linked(func);
            Ok(())
        } else {
            panic!();
        }
    }
}
