use crate::ast::AstNodeRef;
use crate::node::{Ast, Expression, Types};

mod call;
mod value;

pub use call::*;
pub use value::*;

impl Types for AstNodeRef<Expression> {
    fn get_type(&self, _ast: &Ast) {
        println!("Expression")
    }
}
