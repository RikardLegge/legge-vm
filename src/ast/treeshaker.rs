use super::{Ast, NodeID, Result};
use std::collections::{HashSet, VecDeque};
use crate::ast::{NodeBody, NodeReferenceType, NodeValue};
use crate::ast::ast::{NodeReference, SideEffect, SideEffectSet};
use crate::ast::ast::NodeReferenceType::*;

pub fn treeshake(ast: &mut Ast) -> Result<()> {
    let root_id = ast.root();
    TreeShaker::new(ast).shake(root_id)
}

struct TreeShaker<'a> {
    ast: &'a mut Ast,
    stack: HashSet<NodeID>,
}

impl<'a> TreeShaker<'a> {
    fn new(ast: &'a mut Ast) -> Self {
        Self {
            ast,
            stack: HashSet::new(),
        }
    }

    fn has_side_effect(&self, node_id: NodeID) -> Option<SideEffectSet> {
        let node = self.ast.get_node(node_id);
        match node.body {
            NodeBody::ConstValue(NodeValue::RuntimeFn(_)) => Some(SideEffect::Execute.into()),
            _ => None
        }
    }

    fn child_to_parent_reference(child_id: NodeID, body: &NodeBody) -> Option<NodeReferenceType> {
        match body {
            NodeBody::Op { .. }
            | NodeBody::ConstValue(_)
            | NodeBody::VariableValue { .. }
            | NodeBody::PrefixOp { .. }
            | NodeBody::Expression(_) => Some(ReadValue),

            NodeBody::VariableDeclaration { expr, .. }
            if Some(child_id) == *expr => Some(ReadValue),
            NodeBody::VariableDeclaration { expr, .. }
            if Some(child_id) != *expr => Some(WriteValue),

            NodeBody::Import { expr, .. }
            | NodeBody::ConstDeclaration { expr, .. }
            | NodeBody::StaticDeclaration { expr, .. }
            | NodeBody::VariableAssignment { expr, .. }
            if child_id == *expr => Some(ReadValue),
            NodeBody::Import { expr, .. }
            | NodeBody::ConstDeclaration { expr, .. }
            | NodeBody::StaticDeclaration { expr, .. }
            | NodeBody::VariableAssignment { expr, .. }
            if child_id != *expr => Some(WriteValue),

            NodeBody::If { condition, .. } if *condition == child_id => Some(ControlFlow),
            NodeBody::If { body, .. } if *body == child_id => Some(Body),

            | NodeBody::Block { .. }
            | NodeBody::ProcedureDeclaration { .. }
            | NodeBody::Loop { .. } =>
                Some(Body),

            NodeBody::Call { .. } => Some(ExecuteValue),

            NodeBody::Empty
            | NodeBody::Comment(_)
            | NodeBody::TypeDeclaration { .. }
            | NodeBody::Return { .. }
            | NodeBody::Break { .. } => None,

            _ => unreachable!("Invalid node body type {:?}", body)
        }
    }

    fn mark_dependencies_as_active(&mut self, node_id: NodeID) {
        if !self.stack.contains(&node_id) {
            self.stack.insert(node_id);

            let node = self.ast.get_node_mut(node_id);
            node.is_referenced = true;

            let node = self.ast.get_node(node_id);
            match node.body {
                NodeBody::Block { .. } => {}
                _ => {
                    for child_id in node.body.children().cloned().collect::<Vec<NodeID>>() {
                        self.mark_dependencies_as_active(child_id)
                    }
                }
            }

            let node = self.ast.get_node(node_id);
            if let Some(parent_id) = node.parent_id {
                let parent = self.ast.get_node(parent_id);
                if let Some(_) = Self::child_to_parent_reference(node_id, &parent.body) {
                    self.mark_dependencies_as_active(parent_id)
                }
            }

            let node = self.ast.get_node(node_id);
            let references = node.references.iter()
                .cloned()
                .collect::<Vec<NodeReference>>();
            for reference in references {
                self.mark_dependencies_as_active(reference.id)
            }
            self.stack.remove(&node_id);
        }
    }

    fn find_mark_alive_triggers(&mut self, node_id: NodeID, effect: SideEffectSet, triggers: &mut Vec<NodeID>) {
        if !self.stack.contains(&node_id) {
            self.stack.insert(node_id);

            let node = self.ast.get_node(node_id);
            match node.body {
                NodeBody::Call { .. } if effect.is(SideEffect::Execute) => {
                    triggers.push(node_id);
                }
                _ => {}
            }

            let node = self.ast.get_node(node_id);
            if let Some(parent_id) = node.parent_id {
                let parent = self.ast.get_node(parent_id);
                if let Some(_) = Self::child_to_parent_reference(node_id, &parent.body) {
                    self.find_mark_alive_triggers(parent_id, effect, triggers);
                }
            }

            let node = self.ast.get_node(node_id);
            let references = node.referenced_by.iter()
                .cloned()
                .collect::<Vec<NodeReference>>();
            for reference in references {
                self.find_mark_alive_triggers(reference.id, effect, triggers);
            }

            self.stack.remove(&node_id);
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

            if let Some(effect) = self.has_side_effect(node_id) {
                side_effect_nodes.push_back((node_id, effect));
            }
        }

        while let Some((node_id, effect)) = side_effect_nodes.pop_front() {
            let mut triggers = Vec::new();
            self.find_mark_alive_triggers(node_id, effect, &mut triggers);
            for trigger_id in triggers {
                self.mark_dependencies_as_active(trigger_id)
            }
        }

        Ok(())
    }
}
