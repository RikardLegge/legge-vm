use crate::ast::{AstNode, AstNodeRef};
use crate::children::{ChildIterator, Children};
use crate::linker::{LinkContext, Linker};
use crate::node::expression::VariableValueStorage;
use crate::node::statement::ReturnStorage;
use crate::node::{
    Ast, Block, Error, Expression, ExpressionChain, FunctionCall, If, Loop, NodeID, Result,
    Statement, Storage, TypeDeclaration, Variable, VariableValue,
};
use crate::state::State;
use crate::types::{NodeType, NodeUsage, Types};
use std::borrow::Cow;
use std::collections::HashMap;
use std::ops::Deref;

#[derive(Debug)]
pub struct ExpressionChainStorage {
    pub lhs: NodeID<Expression>,
    pub rhs: NodeID<Expression>,
    linked: bool,
}

impl ExpressionChainStorage {
    pub fn new(lhs: NodeID<Expression>, rhs: NodeID<Expression>) -> Self {
        Self {
            lhs,
            rhs,
            linked: false,
        }
    }
}

impl Types for AstNodeRef<ExpressionChain> {
    fn get_type<'this, 'ast>(
        &'this self,
        ast: &'ast Ast,
        usage: NodeUsage,
    ) -> Result<Cow<'ast, NodeType>> {
        let node = ast.body(self.id);
        ast.get(node.rhs).get_type(ast, usage)
    }
}

impl Children for AstNodeRef<ExpressionChain> {
    fn children<'this, 'ast>(&'this self, ast: &'ast Ast) -> ChildIterator<'ast> {
        let node = ast.body(self.id);
        ChildIterator::new([node.lhs.into(), node.rhs.into()].into())
    }
}

impl Linker for AstNodeRef<ExpressionChain> {
    fn link(&self, ast: &mut Ast, _context: LinkContext) -> Result<()> {
        let node = ast.body(self.id);

        if node.linked {
            return Ok(());
        }
        let lhs_id = node.lhs;
        let lhs = ast.get(lhs_id);
        let lhs_variable = match lhs.storage() {
            Storage::VariableValue(value) => {
                let variable_id: NodeID<Variable> = (&value.variable)
                    .try_into()
                    .map_err(|_| Error::UnlinkedNode(lhs_id.into()))?;

                <AstNode<Variable>>::variable_declaration_id(variable_id, ast).map(|_| variable_id)
            }
            _ => None,
        };

        // If the lhs of the expression is a variable, not a type for example,
        // then we want to insert the variable as an implicit first parameter in the call.
        if let Some(variable_id) = lhs_variable {
            let rhs = ast.get(node.rhs);
            if let Ok(rhs) = <&AstNode<FunctionCall>>::try_from(rhs) {
                let rhs_id = rhs.id;
                let value = ast.node_body(
                    Some(node.rhs.into()),
                    VariableValueStorage::new(State::Linked(variable_id)),
                );

                let call = ast.body_mut(rhs_id);
                call.args.insert(0, value.into());
            }
        }

        ast.body_mut(self.id).linked = true;
        Ok(())
    }

    fn link_context(
        &self,
        ast: &Ast,
        child_id: impl Into<NodeID>,
        context: LinkContext,
    ) -> Result<LinkContext> {
        let node = ast.body(self.id);
        if child_id.into() == node.rhs.into() {
            Ok(LinkContext::Node(node.lhs.into()))
        } else {
            Ok(context)
        }
    }
}
