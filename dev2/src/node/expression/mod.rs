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
use crate::node::{Ast, Expression, Result};
use std::borrow::Cow;

use crate::reified;
use crate::types::{NodeType, NodeUsage, Types};

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
