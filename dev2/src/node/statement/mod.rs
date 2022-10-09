use crate::ast::AstNodeRef;
use crate::node::plugins::Types;
use crate::node::{Ast, Statement};
use crate::reified;

impl Types for AstNodeRef<Statement> {
    fn get_type(&self, ast: &Ast) {
        let node = ast.get(self.id);
        reified! {node.get_type(ast)}
    }
}
