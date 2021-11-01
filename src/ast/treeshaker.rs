use super::{Ast, NodeID, Result};
use crate::ast::ast::{Linked, NodeReference, SideEffect};
use crate::ast::nodebody::{NBCall, NBProcedureDeclaration, NodeBody};
use crate::ast::{Err, Node, NodeType, NodeValue};
use std::collections::{HashSet, VecDeque};
use std::result;

pub fn treeshake<T: Linked>(mut ast: Ast<T>) -> result::Result<Ast<T>, (Ast<T>, Err)> {
    let root_id = ast.root();
    let shaker = TreeShaker::new(&mut ast);
    match shaker.shake(root_id) {
        Ok(()) => Ok(ast),
        Err(err) => Err((ast, err)),
    }
}

struct TreeShaker<'a, T>
where
    T: Linked,
{
    ast: &'a mut Ast<T>,
}

struct Shaker<'a, T>
where
    T: Linked,
{
    ast: &'a mut Ast<T>,
    stack: HashSet<NodeID>,
    side_effect_nodes: Vec<(NodeID, SideEffect)>,
    active_roots: Vec<NodeID>,
}

impl<'a, T> Shaker<'a, T>
where
    T: Linked,
{
    fn new(ast: &'a mut Ast<T>) -> Self {
        Self {
            ast,
            stack: Default::default(),
            side_effect_nodes: vec![],
            active_roots: vec![],
        }
    }

    fn child_dependent_on_parent(&self, node: &Node<T>) -> Option<NodeID> {
        if let Some(parent_id) = node.parent_id {
            let parent = self.ast.get_node(parent_id);
            match parent.body {
                NodeBody::Empty
                | NodeBody::Comment(_)
                | NodeBody::TypeDeclaration { .. }
                | NodeBody::Return { .. }
                | NodeBody::Break { .. } => None,
                _ => Some(parent_id),
            }
        } else {
            None
        }
    }

    fn mark_dependencies_as_active(&mut self, node_id: NodeID, effect: &SideEffect) {
        if !self.stack.contains(&node_id) {
            self.stack.insert(node_id);

            let node = self.ast.get_node_mut(node_id);
            node.reference_types.insert(effect.clone());

            let node = self.ast.get_node(node_id);
            match node.body {
                NodeBody::Block { .. } => {}
                _ => {
                    for child_id in node.body.children().cloned().collect::<Vec<NodeID>>() {
                        self.mark_dependencies_as_active(child_id, effect)
                    }
                }
            }

            let node = self.ast.get_node(node_id);
            match node.body {
                NodeBody::VariableValue { variable, .. } => {
                    self.side_effect_nodes.push((variable, SideEffect::Write))
                }
                NodeBody::Loop { .. } => {
                    for reference in &node.referenced_by {
                        self.side_effect_nodes
                            .push((reference.id, SideEffect::GoTo(node_id)))
                    }
                }
                NodeBody::ProcedureDeclaration(NBProcedureDeclaration {
                    ref args,
                    ref returns,
                    ..
                }) => {
                    for reference in &node.referenced_by {
                        self.side_effect_nodes
                            .push((reference.id, SideEffect::GoTo(node_id)));
                    }
                    for arg in args {
                        let arg_node = self.ast.get_node(*arg);
                        for arg_ref in &arg_node.referenced_by {
                            self.side_effect_nodes.push((arg_ref.id, effect.clone()));
                        }
                    }

                    let is_fn = if let Some(returns) = *returns {
                        let return_tp = self.ast.get_node(returns).tp.clone().map(|t| t.tp);
                        if let Some(NodeType::Fn { .. }) = return_tp {
                            true
                        } else {
                            false
                        }
                    } else {
                        false
                    };
                    if is_fn {
                        if let Some(parent_id) = self.child_dependent_on_parent(node) {
                            self.side_effect_nodes.push((
                                parent_id,
                                SideEffect::WhenThen(Box::new((
                                    SideEffect::Execute,
                                    SideEffect::Execute,
                                ))),
                            ))
                        }
                    } else {
                        if let Some(parent_id) = self.child_dependent_on_parent(node) {
                            self.side_effect_nodes
                                .push((parent_id, SideEffect::Execute))
                        }
                    }
                }
                _ => {}
            }

            let node = self.ast.get_node(node_id);
            if let Some(parent_id) = self.child_dependent_on_parent(node) {
                self.mark_dependencies_as_active(parent_id, effect)
            }

            let node = self.ast.get_node(node_id);
            let references = node
                .references
                .iter()
                .cloned()
                .collect::<Vec<NodeReference>>();
            for reference in references {
                self.mark_dependencies_as_active(reference.id, effect)
            }
            self.stack.remove(&node_id);
        }
    }

    fn find_mark_alive_triggers(&mut self, node_id: NodeID, effect: &SideEffect) {
        let effect_is_processed = self.ast.get_node(node_id).reference_types.contains(&effect);
        if !self.stack.contains(&node_id) && !effect_is_processed {
            self.stack.insert(node_id);

            let node = self.ast.get_node(node_id);
            match node.body {
                NodeBody::Call(NBCall { .. }) => {
                    if let SideEffect::Execute = effect {
                        self.active_roots.push(node_id);
                    } else if let SideEffect::WhenThen(when_then) = effect {
                        match &**when_then {
                            (SideEffect::Execute, then) => {
                                self.side_effect_nodes.push((node_id, then.clone()))
                            }
                            _ => {}
                        }
                    }
                }
                NodeBody::VariableAssignment { .. } if effect == &SideEffect::Write => {
                    self.active_roots.push(node_id);
                }
                NodeBody::VariableValue { .. } if effect == &SideEffect::Read => {
                    self.active_roots.push(node_id);
                }
                NodeBody::Break { r#loop } if effect == &SideEffect::GoTo(r#loop) => {
                    self.active_roots.push(node_id);
                }
                NodeBody::Return { func, .. } if effect == &SideEffect::GoTo(func) => {
                    self.active_roots.push(node_id);
                }
                _ => {}
            }

            let node = self.ast.get_node(node_id);
            if let Some(parent_id) = self.child_dependent_on_parent(node) {
                self.find_mark_alive_triggers(parent_id, effect);
            }

            let node = self.ast.get_node(node_id);
            let references = node
                .referenced_by
                .iter()
                .cloned()
                .collect::<Vec<NodeReference>>();
            for reference in references {
                self.find_mark_alive_triggers(reference.id, effect);
            }

            self.stack.remove(&node_id);
        }
    }
}

impl<'a, T> TreeShaker<'a, T>
where
    T: Linked,
{
    fn new(ast: &'a mut Ast<T>) -> Self {
        Self { ast }
    }

    fn has_side_effect(&self, node_id: NodeID) -> Option<SideEffect> {
        let node = self.ast.get_node(node_id);
        match node.body {
            NodeBody::ConstValue {
                value: NodeValue::RuntimeFn(_),
                ..
            } => Some(SideEffect::Execute),
            _ => None,
        }
    }

    fn shake(self, root_id: NodeID) -> Result<()> {
        let mut queue = VecDeque::from(vec![root_id]);
        let mut side_effect_nodes = Vec::new();
        while let Some(node_id) = queue.pop_front() {
            let node = self.ast.get_node(node_id);
            for &child in node.body.children() {
                queue.push_back(child);
            }

            if let Some(effect) = self.has_side_effect(node_id) {
                side_effect_nodes.push((node_id, effect));
            }
        }

        let mut shaker = Shaker::new(self.ast);
        shaker.side_effect_nodes = side_effect_nodes;
        let mut other_active_roots = Vec::new();

        while let Some((node_id, effect)) = shaker.side_effect_nodes.pop() {
            assert_eq!(shaker.active_roots.len(), 0);
            shaker.find_mark_alive_triggers(node_id, &effect);
            std::mem::swap(&mut shaker.active_roots, &mut other_active_roots);
            shaker.active_roots.clear();
            assert_eq!(shaker.stack.len(), 0);

            for active_id in &other_active_roots {
                shaker.mark_dependencies_as_active(*active_id, &effect);
                assert_eq!(shaker.stack.len(), 0);
            }
        }

        Ok(())
    }
}
