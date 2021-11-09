use crate::vm::ast::{Ast, IsLinked, NodeReference, PartialNodeValue, SideEffect};
use crate::vm::ast::{AstBranch, NodeID, Result};
use crate::vm::ast::{NBCall, NBProcedureDeclaration, NodeBody, NodeBodyIterator};
use crate::vm::ast::{Node, NodeReferenceType, NodeType, NodeValue};
use crate::vm::transform;
use std::collections::{HashSet, VecDeque};

pub struct TreeShake {}

impl TreeShake {
    pub fn new() -> Self {
        Self {}
    }
}

impl<T> transform::AstTransformation<T, T> for TreeShake
where
    T: IsLinked,
{
    fn name(&self) -> String {
        "Tree shaker".to_string()
    }
    fn transform(&self, mut ast: Ast<T>) -> transform::Result<T> {
        if true {
            return Ok(ast);
        }
        let mut err = None;
        for mut ast in ast.iter_mut() {
            let root_id = ast.root();
            let shaker = TreeShaker::new(&mut ast);
            if let Err(e) = shaker.shake(root_id) {
                err = Some(e);
                break;
            }
        }

        match err {
            Some(err) => Err((ast.guarantee_state(), err)),
            None => Ok(ast),
        }
    }
}

struct TreeShaker<'a, T>
where
    T: IsLinked,
{
    ast: &'a mut AstBranch<T>,
}

struct Shaker<'a, T>
where
    T: IsLinked,
{
    ast: &'a mut AstBranch<T>,
    stack: HashSet<NodeID>,
    side_effect_nodes: Vec<(NodeID, SideEffect)>,
    active_roots: Vec<NodeID>,
}

impl<'a, T> Shaker<'a, T>
where
    T: IsLinked,
{
    fn new(ast: &'a mut AstBranch<T>) -> Self {
        Self {
            ast,
            stack: Default::default(),
            side_effect_nodes: vec![],
            active_roots: vec![],
        }
    }

    // Safety: the referenced_by field is not allowed to be modified during the lifetime 'b.
    unsafe fn node_referenced_by<'b>(&'a self, node_id: &'b NodeID) -> &'b HashSet<NodeReference> {
        let r = &self.ast.get_node(*node_id).referenced_by;
        let raw = r as *const HashSet<NodeReference>;
        unsafe { &*raw }
    }

    // Safety: the references field is not allowed to be modified during the lifetime 'b.
    unsafe fn node_references<'b>(&'a self, node_id: &'b NodeID) -> &'b HashSet<NodeReference> {
        let r = &self.ast.get_node(*node_id).references;
        let raw = r as *const HashSet<NodeReference>;
        unsafe { &*raw }
    }

    // Safety: the list of node children is not allowed to be modified during the lifetime 'b.
    unsafe fn node_children<'b>(&'a self, node_id: &'b NodeID) -> NodeBodyIterator<'b, T> {
        let body = &self.ast.get_node(*node_id).body;
        let raw = body as *const NodeBody<T>;
        unsafe { (&*raw).children() }
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
            node.reference_types
                .as_mut()
                .unwrap()
                .insert(effect.clone());

            let node = self.ast.get_node(node_id);
            match node.body {
                NodeBody::Block { .. } => {}
                _ => {
                    // Safety: the child ids of the node is not updated in mark_dependencies_as_active.
                    let children = unsafe { self.node_children(&node_id) };
                    for &child_id in children {
                        if child_id.ast() == self.ast.id() {
                            self.mark_dependencies_as_active(child_id, effect)
                        }
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
                        if let Some(NodeType::Fn { .. }) = self.ast.get_node(returns).maybe_tp() {
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

            // Safety: the nodes references table is not updated in mark_dependencies_as_active.
            for reference in unsafe { self.node_references(&node_id) } {
                if reference.ref_tp != NodeReferenceType::ReadExternalValue {
                    self.mark_dependencies_as_active(reference.id, effect)
                }
            }
            self.stack.remove(&node_id);
        }
    }

    fn find_mark_alive_triggers(&mut self, node_id: NodeID, effect: &SideEffect) {
        let effect_is_processed = self
            .ast
            .get_node(node_id)
            .reference_types
            .as_ref()
            .unwrap()
            .contains(&effect);
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

            // Safety: the nodes referenced_by table is not updated in mark_dependencies_as_active.
            for reference in unsafe { self.node_referenced_by(&node_id) } {
                if reference.ref_tp != NodeReferenceType::WriteExternalValue {
                    self.find_mark_alive_triggers(reference.id, effect);
                }
            }

            self.stack.remove(&node_id);
        }
    }
}

impl<'a, T> TreeShaker<'a, T>
where
    T: IsLinked,
{
    fn new(ast: &'a mut AstBranch<T>) -> Self {
        Self { ast }
    }

    fn has_side_effect(&self, node_id: NodeID) -> Option<SideEffect> {
        let node = self.ast.get_node(node_id);
        match node.body {
            NodeBody::ConstValue {
                value: PartialNodeValue::Linked(NodeValue::RuntimeFn(_)),
                ..
            } => Some(SideEffect::Execute),
            _ => None,
        }
    }

    fn shake(self, root_id: NodeID) -> Result<()> {
        let mut queue = VecDeque::from(vec![root_id]);
        let mut side_effect_nodes = Vec::new();
        while let Some(node_id) = queue.pop_front() {
            let node = self.ast.get_node_mut(node_id);
            node.reference_types = Some(HashSet::new());
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
