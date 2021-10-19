use super::{Ast, NodeID, Result};
use std::collections::VecDeque;
use crate::ast::{NodeBody, NodeValue};

pub fn treeshake(ast: &mut Ast) -> Result<()> {
    let root_id = ast.root();
    TreeShaker::new(ast).shake(root_id)
}

struct TreeShaker<'a> {
    ast: &'a mut Ast,
}

impl<'a> TreeShaker<'a> {
    fn new(ast: &'a mut Ast) -> Self {
        Self {
            ast,
        }
    }

    fn has_side_effect(&self, node_id: NodeID) -> bool {
        let node = self.ast.get_node(node_id);
        match node.body {
            NodeBody::ConstValue(NodeValue::RuntimeFn(_)) => true,
            _ => false
        }
    }

    fn mark_as_alive(&mut self, node_id: NodeID) {
        let node = self.ast.get_node_mut(node_id);
        if node.is_dead.is_none() {
            node.is_dead = Some(false);

            let node = self.ast.get_node(node_id);
            match node.body {
                NodeBody::Block { .. } => {}
                _ => {
                    for child_id in node.body.children().cloned().collect::<Vec<NodeID>>() {
                        self.mark_as_alive(child_id);
                    }
                }
            }

            let node = self.ast.get_node(node_id);
            if let Some(parent_id) = node.parent_id {
                let parent = self.ast.get_node(parent_id);
                match parent.body {
                    NodeBody::Op { .. }
                    | NodeBody::ConstValue(_)
                    | NodeBody::PrefixOp { .. }
                    | NodeBody::Block { .. }
                    | NodeBody::Expression(_)
                    | NodeBody::Import { .. }
                    | NodeBody::VariableDeclaration { .. }
                    | NodeBody::ConstDeclaration { .. }
                    | NodeBody::StaticDeclaration { .. }
                    | NodeBody::VariableAssignment { .. }
                    | NodeBody::ProcedureDeclaration { .. }
                    | NodeBody::If { .. }
                    | NodeBody::VariableValue { .. }
                    | NodeBody::Loop { .. } => self.mark_as_alive(parent_id),

                    NodeBody::Empty
                    | NodeBody::Comment(_)
                    | NodeBody::TypeDeclaration { .. }
                    | NodeBody::Return { .. }
                    | NodeBody::Break { .. }
                    | NodeBody::Call { .. } => {}

                    NodeBody::Unlinked(_) => unreachable!()
                }
            }

            let node = self.ast.get_node(node_id);
            let references = node.references.iter()
                .map(|r| r.id)
                .collect::<Vec<NodeID>>();
            for dep_id in references {
                self.mark_as_alive(dep_id);
            }

            let node = self.ast.get_node(node_id);
            let references = node.referenced_by.iter()
                .map(|r| r.id)
                .collect::<Vec<NodeID>>();
            for dep_id in references {
                self.mark_as_alive(dep_id);
            }
        }
    }

    fn shake(mut self, root_id: NodeID) -> Result<()> {
        let mut queue = VecDeque::from(vec![root_id]);
        let mut side_effect_nodes = VecDeque::new();
        while let Some(node_id) = queue.pop_front() {
            let node = self.ast.get_node(node_id);
            for &child in node.body.children() {
                queue.push_back(child);
            }

            if self.has_side_effect(node_id) {
                side_effect_nodes.push_back(node_id);
            }
        }

        while let Some(node_id) = side_effect_nodes.pop_front() {
            self.mark_as_alive(node_id);
        }

        let mut queue = VecDeque::from(vec![root_id]);
        while let Some(node_id) = queue.pop_front() {
            let node = self.ast.get_node(node_id);
            for &child in node.body.children() {
                queue.push_back(child);
            }

            if let None = node.is_dead {
                let node = self.ast.get_node_mut(node_id);
                node.is_dead = Some(true);
            }
        }

        Ok(())
    }
}
