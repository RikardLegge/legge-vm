use crate::ast::AstNodeRef;
use crate::node::{Any, Ast, Statement};
use crate::reified;
use crate::types::Types;

impl Types<Any> for AstNodeRef<Statement> {
    fn get_type(&self, ast: &Ast) {
        let node = ast.get(self.id);
        reified! {node.get_type(ast)}
    }
}
