use crate::ast::AstNodeRef;
use crate::node::{Ast, Call, Types};

impl Types for AstNodeRef<Call> {
    fn get_type(&self, _ast: &Ast) {
        println!("Call")
    }
}

pub struct CallData();
