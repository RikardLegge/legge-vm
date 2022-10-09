mod block;
mod function_declaration;
mod r#loop;
mod variable_value;

pub use block::*;
pub use function_declaration::*;
pub use r#loop::*;
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
