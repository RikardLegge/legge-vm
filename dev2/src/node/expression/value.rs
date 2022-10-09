use crate::ast::AstNodeRef;
use crate::node::{Any, Ast, Types, Value};

impl Types<Any> for AstNodeRef<Value> {
    fn get_type(&self, _ast: &Ast) {
        println!("Value")
    }
}

pub struct ValueStorage();
