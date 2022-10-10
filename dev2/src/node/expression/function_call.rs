use crate::ast::AstNodeRef;
use crate::children::{ChildIterator, Children};
use crate::node::{
    Ast, Block, Expression, FunctionCall, FunctionDeclaration, NodeID, Result, Variable,
    VariableValue,
};
use crate::types::{NodeType, NodeUsage, Types};
use std::borrow::{Borrow, Cow};
use std::ops::Deref;

#[derive(Debug)]
pub struct FunctionCallStorage {
    pub variable: NodeID<VariableValue>,
    pub args: Vec<NodeID<Expression>>,
}

impl FunctionCallStorage {
    pub fn new(variable: NodeID<VariableValue>, args: Vec<NodeID<Expression>>) -> Self {
        Self { variable, args }
    }
}

impl Types for AstNodeRef<FunctionCall> {
    fn get_type<'this, 'ast>(
        &'this self,
        ast: &'ast Ast,
        usage: NodeUsage,
    ) -> Result<Cow<'ast, NodeType>> {
        let node = ast.body(self.id);
        let value = ast.body(node.variable);
        let variable_id = (&value.variable).try_into().unwrap();
        let variable = ast.get(variable_id);
        let variable_type = variable.get_type(ast, usage)?;
        if let NodeType::Function(func) = variable_type.borrow() {
            match usage {
                NodeUsage::Type => ast.get(*func).get_type(ast, usage),
                NodeUsage::Value => Ok(Cow::Borrowed(&ast.body(*func).returns)),
            }
        } else {
            unimplemented!()
        }
    }
}

impl Children for AstNodeRef<FunctionCall> {
    fn children<'this, 'ast>(&'this self, ast: &'ast Ast) -> ChildIterator<'ast> {
        let node = ast.body(self.id);
        ChildIterator::new([node.variable.into(), node.args.deref().into()].into())
    }
}
