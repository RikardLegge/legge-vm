use crate::ast::AstNodeRef;
use crate::node::{Ast, Types, Value};

impl Types for AstNodeRef<Value> {
    fn get_type(&self, _ast: &Ast) {
        println!("Value")
    }
}

pub struct ValueData();
