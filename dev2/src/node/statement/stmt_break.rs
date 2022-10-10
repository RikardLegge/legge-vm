use crate::ast::{AstNode, AstNodeRef};
use crate::children::{ChildIterator, Children};
use crate::linker::{Linker, LinkerContext, LinkerExt};
use crate::node::{
    Ast, Break, Expression, FunctionDeclaration, Loop, NodeID, Result, Return, TypeDeclaration,
    Variable,
};
use crate::state::State;
use crate::types::{NodeType, NodeUsage, Types};
use std::borrow::Cow;

#[derive(Debug)]
pub struct BreakStorage {
    pub r#loop: State<(), NodeID<Loop>>,
    pub value: Option<NodeID<Expression>>,
}

impl BreakStorage {
    pub fn new(value: Option<NodeID<Expression>>) -> Self {
        Self {
            r#loop: ().into(),
            value,
        }
    }
}

impl Types for AstNodeRef<Break> {
    fn get_type<'this, 'ast>(
        &'this self,
        _ast: &'ast Ast,
        _usage: NodeUsage,
    ) -> Result<Cow<'ast, NodeType>> {
        Ok(Cow::Owned(NodeType::Void))
    }
}

impl Children for AstNodeRef<Break> {
    fn children<'this, 'ast>(&'this self, ast: &'ast Ast) -> ChildIterator<'ast> {
        let node = ast.body(self.id);
        match node.value {
            Some(value) => ChildIterator::new(([value.into()].into())),
            None => ChildIterator::new([].into()),
        }
    }
}

impl Linker for AstNodeRef<Break> {
    fn link(&self, ast: &mut Ast, context: LinkerContext) -> Result<()> {
        let node = ast.body(self.id);
        let loop_id = match node.r#loop {
            State::Unlinked(_) => {
                let loop_id =
                    ast.walk_up(self.id, |node| match <&AstNode<Loop>>::try_from(node) {
                        Ok(node) => Ok(Some(node.id)),
                        Err(_) => Ok(None),
                    })?;
                if let Some(loop_id) = loop_id {
                    ast.body_mut(self.id).r#loop = State::Linked(loop_id);
                    loop_id
                } else {
                    unimplemented!();
                }
            }
            State::Linked(id) => id,
        };

        let loop_node = ast.body(loop_id);
        if loop_node.value.is_none() {
            let node = ast.body(self.id);
            let tp = if let Some(value) = node.value {
                ast.get(value).get_type(ast, NodeUsage::Value)?.into_owned()
            } else {
                NodeType::Void
            };
            let loop_node = ast.body_mut(loop_id);
            loop_node.value = Some(tp);
        };

        Ok(())
    }
}
