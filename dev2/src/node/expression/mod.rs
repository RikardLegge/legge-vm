mod block;
mod expr_chain;
mod expr_if;
mod expr_loop;
mod function_call;
mod function_declaration;
mod operation;
mod value;
mod variable_value;

pub use block::*;
pub use expr_chain::*;
pub use expr_if::*;
pub use expr_loop::*;
pub use function_call::*;
pub use function_declaration::*;
pub use operation::*;
pub use value::*;
pub use variable_value::*;

use crate::ast::AstNodeRef;
use crate::linker::{LinkContext, Linker};
use crate::node::{Ast, Expression, Result};
use std::borrow::Cow;

use crate::children::{ChildIterator, Children};
use crate::types::{NodeType, NodeUsage, Types};
use crate::{ast, reified};

impl Types for AstNodeRef<Expression> {
    fn get_type<'this, 'ast>(
        &'this self,
        ast: &'ast Ast,
        usage: NodeUsage,
    ) -> Result<Cow<'ast, NodeType>> {
        let node = ast.get(self.id);
        reified! {node.get_type(ast, usage)}
    }
}

impl Children for AstNodeRef<Expression> {
    fn children<'this, 'ast>(&'this self, ast: &'ast Ast) -> ChildIterator<'ast> {
        let node = ast.get(self.id);
        reified! {node.children(ast)}
    }
}

impl Linker for AstNodeRef<Expression> {
    fn link(&self, ast: &mut Ast, context: LinkContext) -> Result<()> {
        let node = ast.get(self.id);
        reified! {node.link(ast, context)}
    }
}
