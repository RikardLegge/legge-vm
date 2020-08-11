use super::{Ast, Node, Result};

pub fn check_types(ast: &Ast) -> Result<()> {
    Checker::new(ast).check_all_types()
}

pub struct Checker<'a> {
    ast: &'a Ast,
}

impl<'a> Checker<'a> {
    pub fn new(ast: &'a Ast) -> Self {
        Self { ast }
    }

    pub fn check_type(&self, node: &Node) -> Result<()> {
        if let Some(_) = &node.tp {
            Ok(())
        } else {
            Err(self.ast.error(
                "Encountered a node without a type",
                "expression with missing type",
                vec![node.id],
            ))
        }
    }

    pub fn check_all_types(&self) -> Result<()> {
        let root_id = self.ast.root();
        let root = self.ast.get_node(root_id);
        self.check_type(root)?;
        for child_id in root.body.children() {
            let child = self.ast.get_node(*child_id);
            self.check_type(child)?;
        }
        Ok(())
    }
}
