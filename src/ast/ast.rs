use super::Result;
use crate::ast::nodebody::{NBProcedureDeclaration, NodeBody};
use crate::ast::Err;
use crate::token::Token;
use std::collections::HashSet;
use std::fmt::{Debug, Formatter};
use std::marker::PhantomData;
use std::{fmt, mem};

#[derive(Default, Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct NodeID(usize);

impl NodeID {
    pub fn new(id: usize) -> Self {
        NodeID(id)
    }

    pub fn zero() -> Self {
        NodeID(0)
    }
}

pub struct CallNode(NodeID);

impl CallNode {
    pub fn id(&self) -> NodeID {
        self.0
    }

    pub fn tp(&self, ast: &Ast) -> Option<NodeType> {
        if let Some(InferredType { tp, .. }) = &ast.get_node(self.id()).tp {
            Some(tp.clone())
        } else {
            let args = {
                let arg_ids = self.args(ast);
                let mut args = Vec::with_capacity(arg_ids.len());
                for id in arg_ids {
                    match &ast.get_node(*id).tp {
                        Some(InferredType { tp, .. }) => args.push(tp.clone()),
                        None => return None,
                    }
                }
                args
            };
            let returns = {
                let function = ast.get_node(self.target(ast));
                unimplemented!()
                // let returns = match function {
                //     NodeType::Fn { returns, .. } => returns,
                //     NodeType::NewType { tp, .. } => tp,
                //     _ => unimplemented!(),
                // };
                // returns.clone()
            };
            Some(NodeType::Fn { args, returns })
        }
    }

    pub fn target(&self, ast: &Ast) -> NodeID {
        match &ast.get_node(self.id()).body {
            NodeBody::Call(call) => call.func,
            _ => unreachable!(),
        }
    }

    pub fn args<'a>(&'_ self, ast: &'a Ast) -> &'a [NodeID] {
        match &ast.get_node(self.id()).body {
            NodeBody::Call(call) => &call.args,
            _ => unreachable!(),
        }
    }

    pub fn from<T: Debug>(node: &Node<T>) -> Option<CallNode> {
        match node.body {
            NodeBody::Call(_) => Some(CallNode(node.id)),
            _ => None,
        }
    }
}

pub struct FnNode {
    pub id: NodeID,
}

#[derive(Debug, Clone)]
pub struct Node<T = state::StateAny>
where
    T: Debug,
{
    pub id: NodeID,
    pub tokens: Vec<Token>,
    pub tp: Option<InferredType>,
    pub body: NodeBody,
    pub parent_id: Option<NodeID>,
    pub referenced_by: HashSet<NodeReference>,
    pub references: HashSet<NodeReference>,
    pub reference_types: HashSet<SideEffect>,
    _tp: PhantomData<T>,
}

impl<T> Node<T>
where
    T: Debug,
{
    pub fn is_closure_boundary(&self) -> bool {
        match self.body {
            NodeBody::ProcedureDeclaration(NBProcedureDeclaration { .. }) => true,
            _ => false,
        }
    }

    pub fn is_dead(&self) -> bool {
        self.reference_types.is_empty()
    }

    pub fn has_closure_references(&self) -> bool {
        self.referenced_by
            .iter()
            .any(|ref_by| ref_by.ref_loc == NodeReferenceLocation::Closure)
    }

    pub fn child_tokens(&self, ast: &Ast<T>) -> Vec<Token> {
        let mut tokens = Vec::new();
        for child_id in self.body.children() {
            let child = ast.get_node(*child_id);
            tokens.append(&mut child.tokens.clone());
            tokens.append(&mut child.child_tokens(ast))
        }
        tokens
    }
}

impl<T> Node<T>
where
    T: TypesInferred + Debug,
{
    pub fn inferred_tp(&self) -> &InferredType {
        self.tp.as_ref().unwrap()
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct NodeReference {
    pub id: NodeID,
    pub ref_tp: NodeReferenceType,
    pub ref_loc: NodeReferenceLocation,
}

impl NodeReference {
    pub fn new(id: NodeID, ref_tp: NodeReferenceType, ref_loc: NodeReferenceLocation) -> Self {
        Self {
            id,
            ref_tp,
            ref_loc,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum SideEffect {
    Read,
    Write,
    Execute,
    WhenThen(Box<(SideEffect, SideEffect)>),
    GoTo(NodeID),
}

impl Into<SideEffectSet> for SideEffect {
    fn into(self) -> SideEffectSet {
        let mut set = SideEffectSet::new();
        set.insert(self);
        set
    }
}

#[derive(Clone, Eq, PartialEq)]
pub struct SideEffectSet(HashSet<SideEffect>);

impl Debug for SideEffectSet {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.is_empty() {
            write!(f, "-")?;
        }
        if self.contains(SideEffect::Read) {
            write!(f, "R")?;
        }
        if self.contains(SideEffect::Write) {
            write!(f, "W")?;
        }
        if self.contains(SideEffect::Execute) {
            write!(f, "X")?;
        }
        Ok(())
    }
}

impl SideEffectSet {
    pub fn new() -> SideEffectSet {
        SideEffectSet(HashSet::new())
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn contains(&self, side_effect: SideEffect) -> bool {
        self.0.contains(&side_effect)
    }

    pub fn contains_all(&self, other: SideEffectSet) -> bool {
        for item in other.0 {
            if !self.contains(item) {
                return false;
            }
        }
        true
    }

    pub fn extend(&mut self, set: SideEffectSet) {
        self.0.extend(set.0);
    }

    pub fn insert(&mut self, item: SideEffect) {
        self.0.insert(item);
    }

    pub fn remove(&mut self, item: SideEffect) {
        self.0.remove(&item);
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum NodeReferenceType {
    ReadValue,
    WriteValue,
    ExecuteValue,
    GoTo,
    ControlFlow,
    Body,
    External,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum NodeReferenceLocation {
    Local,
    Closure,
}

#[derive(Debug, Clone, PartialEq)]
pub struct InferredType {
    pub source: NodeTypeSource,
    pub tp: NodeType,
}

impl InferredType {
    pub fn new(tp: NodeType, source: NodeTypeSource) -> Self {
        Self { tp, source }
    }

    pub fn maybe(tp: Option<NodeType>, source: NodeTypeSource) -> Option<Self> {
        match tp {
            Some(tp) => Some(Self::new(tp, source)),
            None => None,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub enum NodeTypeSource {
    Usage,
    Value,
    Variable,
    Declared,
}

#[derive(Debug, Clone, PartialEq)]
pub enum NodeType {
    VarArg {
        args: Box<NodeType>,
    },
    Any,
    NotYetImplemented,
    Void,
    Int,
    Float,
    Bool,
    String,
    Fn {
        args: Vec<NodeType>,
        returns: Box<NodeType>,
    },
    NewType {
        tp: Box<NodeType>,
    },
    Type {
        ident: String,
        content: Box<NodeType>,
    },
    Unknown {
        ident: String,
    },
    Struct {
        fields: Vec<(String, NodeType)>,
    },
}

#[derive(Debug, Clone)]
pub enum NodeValue {
    Int(isize),
    Float(f64),
    Bool(bool),
    String(String),
    RuntimeFn(usize),
    Struct(Vec<(String, NodeValue)>),
    Unlinked(String),
}

#[derive(Debug, Clone)]
pub enum PartialType {
    Complete(NodeType),
    Uncomplete(NodeType),
}

impl PartialType {
    pub fn new(complete: bool, tp: NodeType) -> Self {
        if complete {
            Self::Complete(tp)
        } else {
            Self::Uncomplete(tp)
        }
    }

    pub fn is_complete(&self) -> bool {
        match self {
            PartialType::Complete(_) => true,
            PartialType::Uncomplete(_) => false,
        }
    }

    pub fn option(&self) -> Option<&NodeType> {
        match self {
            PartialType::Complete(tp) => Some(tp),
            PartialType::Uncomplete(_) => None,
        }
    }

    pub fn tp(&self) -> &NodeType {
        match self {
            PartialType::Complete(tp) | PartialType::Uncomplete(tp) => tp,
        }
    }
}

mod state {
    pub trait Any {}
    pub trait Linked {}
    pub trait TypesInferred {}
    pub trait TypesChecked {}

    #[derive(Debug, Copy, Clone)]
    pub struct StateAny {}
    impl Any for StateAny {}

    #[derive(Debug, Copy, Clone)]
    pub struct StateLinked {}
    impl Linked for StateLinked {}
    impl Any for StateLinked {}

    #[derive(Debug, Copy, Clone)]
    pub struct StateTypesInferred {}
    impl TypesInferred for StateTypesInferred {}
    impl Linked for StateTypesInferred {}
    impl Any for StateTypesInferred {}

    #[derive(Debug, Copy, Clone)]
    pub struct StateTypesChecked {}
    impl TypesChecked for StateTypesChecked {}
    impl TypesInferred for StateTypesChecked {}
    impl Linked for StateTypesChecked {}
    impl Any for StateTypesChecked {}
}

pub use state::{Any, Linked, TypesChecked, TypesInferred};
pub use state::{StateAny, StateLinked, StateTypesChecked, StateTypesInferred};

pub struct Ast<T = state::StateAny> {
    nodes: Vec<Node<state::StateAny>>,
    root: NodeID,
    _tp: PhantomData<T>,
}

impl<T> fmt::Debug for Ast<T>
where
    T: Debug,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.fmt_debug_node(f, 0, self.root)?;
        write!(f, "\n")
    }
}

impl<T> Ast<T>
where
    T: Debug,
{
    pub fn unimplemented(&self, id: NodeID) -> Result {
        Err(Err::new(self, "Unimplemented", "", vec![id]))
    }

    pub fn error(&self, details: &str, row_details: &str, nodes: Vec<NodeID>) -> Err {
        Err::new(self, details, row_details, nodes)
    }

    pub fn new() -> Self {
        Self {
            root: NodeID::zero(),
            nodes: Vec::new(),
            _tp: PhantomData::default(),
        }
    }

    pub fn nodes(&self) -> &[Node<T>] {
        unsafe { mem::transmute::<&[Node], &[Node<T>]>(&self.nodes) }
    }

    pub fn add_ref(
        &mut self,
        target: (NodeID, NodeReferenceType),
        referencer: (NodeID, NodeReferenceType),
        loc: NodeReferenceLocation,
    ) {
        let referenced_by = NodeReference::new(referencer.0, referencer.1, loc);
        let inserted_referenced = self
            .get_node_mut(target.0)
            .referenced_by
            .insert(referenced_by);

        let references = NodeReference::new(target.0, target.1, loc);
        let inserted_references = self
            .get_node_mut(referencer.0)
            .references
            .insert(references);

        if !(inserted_referenced && inserted_references) {
            format!("WARNING: inserted already existing reference!");
        }
    }

    pub fn remove_ref(
        &mut self,
        target_id: NodeID,
        referencer_id: NodeID,
        tp: NodeReferenceType,
        loc: NodeReferenceLocation,
    ) {
        let referenced_by = NodeReference::new(referencer_id, tp, loc);
        let removed_referenced = self
            .get_node_mut(target_id)
            .referenced_by
            .remove(&referenced_by);

        let references = NodeReference::new(target_id, tp, loc);
        let removed_referencer = self
            .get_node_mut(referencer_id)
            .references
            .remove(&references);
        if !removed_referenced && !removed_referencer {
            format!("WARNING: reference missing between target {:?} and referer {:?} during reference removal", target_id, referencer_id);
        } else if !removed_referenced && removed_referencer {
            format!("WARNING: reference missing from target {:?} to referer {:?} during reference removal", target_id, referencer_id);
        } else if removed_referenced && !removed_referencer {
            format!("WARNING: removed reference missing to target {:?} from referer {:?} during reference removal", target_id, referencer_id);
        }
    }

    pub fn add_root_node(&mut self) -> NodeID {
        self.add_some_node(None)
    }

    pub fn add_node(&mut self, parent_id: NodeID) -> NodeID {
        self.add_some_node(Some(parent_id))
    }

    fn add_some_node(&mut self, parent_id: Option<NodeID>) -> NodeID {
        let node = Node {
            id: NodeID::new(self.nodes.len()),
            parent_id,
            referenced_by: Default::default(),
            references: Default::default(),
            tp: None,
            tokens: vec![],
            body: NodeBody::Empty,
            reference_types: HashSet::new(),
            _tp: PhantomData::default(),
        };
        let id = node.id;
        self.nodes.push(node);
        id
    }

    pub fn root(&self) -> NodeID {
        self.root
    }

    fn fmt_debug_node(&self, f: &mut Formatter<'_>, level: usize, node_id: NodeID) -> fmt::Result {
        if level > 100 {
            write!(f, "Nesting level too deep!")?;
            return Ok(());
        }
        let node = &self.nodes[node_id.0];
        let mut children = node.body.children().peekable();

        let prefix = if node.is_dead() { "-" } else { " " };
        write!(f, "{}", prefix)?;

        let pad_len = 2 + level * 2;
        let pad_start =
            " ".repeat(std::cmp::max(0, pad_len as isize - prefix.len() as isize) as usize);
        let pad_end = " ".repeat(pad_len);
        write!(f, "{}", pad_start)?;

        write!(f, " Node({}):", node.id.0)?;
        if node.has_closure_references() {
            write!(f, " closure ")?;
        }
        if let Some(InferredType { source, tp }) = &node.tp {
            if !node.has_closure_references() {
                write!(f, " ")?;
            }
            write!(f, "{{ {:?}, {:?} }} ", source, tp)?;
        }
        write!(f, "= {:?}", node.body)?;

        if children.peek().is_some() {
            write!(f, " [\n")?;
            for &child in children {
                match &self.get_node(child).body {
                    NodeBody::Comment(..) => continue,
                    _ => (),
                }
                self.fmt_debug_node(f, level + 1, child)?;
                write!(f, "\n")?;
            }
            if node.is_dead() {
                write!(f, "-")?;
            } else {
                write!(f, " ")?;
            }
            write!(f, "{}]", pad_end)?;
        }
        Ok(())
    }

    pub fn get_node(&self, node_id: NodeID) -> &Node<T> {
        match self.nodes.get(node_id.0) {
            Some(node) => unsafe { mem::transmute::<&Node, &Node<T>>(node) },
            None => panic!("Could not find Node({}) in ast", node_id.0),
        }
    }

    pub fn get_node_type(&self, node_id: NodeID) -> Option<NodeType> {
        self.get_node(node_id).tp.clone().map(|t| t.tp)
    }

    pub fn get_node_mut(&mut self, node_id: NodeID) -> &mut Node<T> {
        match self.nodes.get_mut(node_id.0) {
            Some(node) => unsafe { mem::transmute::<&mut Node, &mut Node<T>>(node) },
            None => panic!("Could not find Node({}) in ast", node_id.0),
        }
    }

    pub fn closest_fn(&self, node_id: NodeID) -> Option<(NodeID, NodeReferenceLocation)> {
        self.closest(node_id, |node| {
            Ok(match node.body {
                NodeBody::ProcedureDeclaration(NBProcedureDeclaration { .. }) => Some(node.id),
                _ => None,
            })
        })
        .unwrap()
    }

    pub fn closest_loop(&self, node_id: NodeID) -> Option<(NodeID, NodeReferenceLocation)> {
        self.closest(node_id, |node| {
            Ok(match node.body {
                NodeBody::Loop { .. } => Some(node.id),
                _ => None,
            })
        })
        .unwrap()
    }

    pub fn closest_variable(
        &self,
        node_id: NodeID,
        target_ident: &str,
    ) -> Result<Option<(NodeID, NodeReferenceLocation)>> {
        use crate::ast::nodebody::NodeBody::*;
        self.closest(node_id, |node| {
            let mut closest_id = None;
            for &child_id in node.body.children() {
                let child = self.get_node(child_id);
                match &child.body {
                    VariableDeclaration { ident, .. }
                    | ConstDeclaration { ident, .. }
                    | StaticDeclaration { ident, .. }
                    | TypeDeclaration { ident, .. }
                    | Import { ident, .. } => {
                        if ident == target_ident {
                            if let Some(closest_id) = closest_id {
                                return Err(self.error(
                                    "Multiple variable declarations with same name encountered",
                                    "Variable declaration",
                                    vec![closest_id, child_id],
                                ));
                            }
                            closest_id = Some(child_id);
                        }
                    }
                    _ => (),
                }
            }
            Ok(closest_id)
        })
    }

    pub fn closest<F>(
        &self,
        mut node_id: NodeID,
        test: F,
    ) -> Result<Option<(NodeID, NodeReferenceLocation)>>
    where
        F: Fn(&Node<T>) -> Result<Option<NodeID>>,
    {
        let mut location = NodeReferenceLocation::Local;
        loop {
            let node = self.get_node(node_id);
            let result = test(node);
            match result {
                Ok(Some(node_id)) => {
                    break Ok(Some((node_id, location)));
                }
                Ok(_) => {
                    if let Some(parent_id) = node.parent_id {
                        if node.is_closure_boundary() {
                            location = NodeReferenceLocation::Closure;
                        }
                        node_id = parent_id;
                    } else {
                        break Ok(None);
                    }
                }
                Err(e) => break Result::Err(e),
            }
        }
    }

    pub fn partial_type(&self, node_id: NodeID) -> Option<(NodeID, &NodeType)> {
        let node = self.get_node(node_id);
        match &node.body {
            NodeBody::TypeDeclaration { tp, .. } | NodeBody::TypeReference { tp } => {
                self.partial_type(*tp)
            }
            NodeBody::PartialType { tp, .. } => {
                let tp = match tp.tp() {
                    NodeType::NewType { tp, .. } => tp,
                    tp => tp,
                };
                Some((node_id, tp))
            }
            _ => None,
        }
    }
}
