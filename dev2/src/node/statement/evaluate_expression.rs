use crate::ast::{AstNode, AstNodeRef};
use crate::children::{ChildIterator, Children};
use crate::node::{
    Ast, Break, EvaluateExpression, Expression, FunctionDeclaration, Loop, NodeID, Result, Return,
    TypeDeclaration, Variable,
};
use crate::state::State;
use crate::types::{NodeType, NodeUsage, Types};
use std::borrow::Cow;

#[derive(Debug)]
pub struct EvaluateExpressionStorage {
    pub value: NodeID<Expression>,
    pub has_end_statement: bool,
}

impl EvaluateExpressionStorage {
    pub fn new(value: NodeID<Expression>) -> Self {
        Self {
            value,
            has_end_statement: false,
        }
    }
}

impl Types for AstNodeRef<EvaluateExpression> {
    fn get_type<'this, 'ast>(
        &'this self,
        ast: &'ast Ast,
        usage: NodeUsage,
    ) -> Result<Cow<'ast, NodeType>> {
        let node = ast.body(self.id);
        ast.get(node.value).get_type(ast, usage)
    }
}

impl Children for AstNodeRef<EvaluateExpression> {
    fn children<'this, 'ast>(&'this self, ast: &'ast Ast) -> ChildIterator<'ast> {
        let node = ast.body(self.id);
        ChildIterator::new([node.value.into()].into())
    }
}
