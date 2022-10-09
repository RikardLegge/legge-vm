use crate::ast::AstNodeRef;
use crate::node::{Any, Ast, Expression, Types};

mod call;
mod value;

use crate::reified;
pub use call::*;
pub use value::*;

impl Types<Any> for AstNodeRef<Expression> {
    fn get_type(&self, ast: &Ast) {
        let node = ast.get(self.id);
        reified! {node.get_type(ast)}
    }
}
