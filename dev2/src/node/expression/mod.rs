use crate::ast::AstNodeRef;
use crate::node::{Ast, Expression, Types};

mod call;
mod value;

use crate::reified;
pub use call::*;
pub use value::*;

impl Types for AstNodeRef<Expression> {
    fn get_type(&self, ast: &Ast) {
        let node = ast.get(self.id);
        reified! {node.get_type(ast)}
    }
}
