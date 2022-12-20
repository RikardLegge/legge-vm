use crate::ast::AstNodeRef;
use crate::node::{Any, Ast, Call, Types};

impl Types<Any> for AstNodeRef<Call> {
    fn get_type(&self, _ast: &Ast) {
        println!("Call")
    }
}

pub struct CallStorage();
