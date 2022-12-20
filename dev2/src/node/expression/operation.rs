use crate::ast::{AstNode, AstNodeRef};
use crate::children::{ChildIterator, Children};
use crate::linker::Linker;
use crate::node::expression::ExpressionChainStorage;
use crate::node::statement::ReturnStorage;
use crate::node::{
    Ast, Block, Expression, ExpressionChain, If, Loop, NodeID, Operation, Result, Statement,
    TypeDeclaration, Variable,
};
use crate::state::State;
use crate::token::ArithmeticOP;
use crate::types::{NodeType, NodeUsage, Types};
use std::borrow::Cow;
use std::collections::HashMap;
use std::ops::Deref;

#[derive(Debug)]
pub struct OperationStorage {
    pub op: ArithmeticOP,
    pub lhs: NodeID<Expression>,
    pub rhs: NodeID<Expression>,
}

impl OperationStorage {
    pub fn new(op: ArithmeticOP, lhs: NodeID<Expression>, rhs: NodeID<Expression>) -> Self {
        Self { op, lhs, rhs }
    }
}

impl Types for AstNodeRef<Operation> {
    fn get_type<'this, 'ast>(
        &'this self,
        ast: &'ast Ast,
        usage: NodeUsage,
    ) -> Result<Cow<'ast, NodeType>> {
        let op = ast.body(self.id);
        let lhs_type = ast.get(op.lhs).get_type(ast, usage)?;
        let rhs_type = ast.get(op.lhs).get_type(ast, usage)?;
        if lhs_type == rhs_type {
            match op.op {
                ArithmeticOP::Add | ArithmeticOP::Sub | ArithmeticOP::Mul | ArithmeticOP::Div => {
                    Ok(lhs_type)
                }
                ArithmeticOP::Eq | ArithmeticOP::GEq | ArithmeticOP::LEq => {
                    Ok(Cow::Owned(NodeType::Boolean))
                }
            }
        } else {
            unimplemented!();
            // Err(Error::TypeMissmatch(op.lhs.into(), op.rhs.into()))
        }
    }
}

impl Children for AstNodeRef<Operation> {
    fn children<'this, 'ast>(&'this self, ast: &'ast Ast) -> ChildIterator<'ast> {
        let node = ast.body(self.id);
        ChildIterator::new([node.lhs.into(), node.rhs.into()].into())
    }
}

impl Linker for AstNodeRef<Operation> {}
