use super::Result;
use crate::ast::nodebody::{NBProcedureDeclaration, NodeBody};
use crate::ast::{Err, Path, PathKey};
use crate::token::Token;
use std::cell::{Ref, RefCell};
use std::collections::{HashMap, HashSet};
use std::fmt::{Debug, Formatter};
use std::marker::PhantomData;
use std::ops::Deref;
use std::{fmt, mem};

#[derive(Default, Debug, Copy, Clone, PartialEq, Eq, Ord, PartialOrd, Hash)]
pub struct AstID(usize);

impl AstID {
    pub fn new(id: usize) -> Self {
        Self(id)
    }
}

#[derive(Debug)]
pub struct AstCollection<T = state::StateAny>
where
    T: Debug,
{
    asts: Vec<RefCell<Ast<T>>>,
    names: HashMap<PathKey, AstID>,
}

pub struct AstGuard<'a, T>(NodeID, Ref<'a, Ast<T>>)
where
    T: Debug;

impl<'a, T> Deref for AstGuard<'a, T>
where
    T: Debug,
{
    type Target = Node<T>;

    fn deref(&self) -> &Node<T> {
        &self.1.get_node(self.0)
    }
}

impl<T> AstCollection<T>
where
    T: Debug,
{
    pub fn new() -> Self {
        Self {
            asts: vec![],
            names: Default::default(),
        }
    }

    pub fn reserve(&mut self) -> AstID {
        let id = AstID::new(self.asts.len());
        self.asts
            .push(RefCell::new(Ast::new("...".to_string(), id)));
        id
    }

    pub fn root(&self) -> NodeID {
        self.asts[0].borrow().root()
    }

    pub fn get_node(&self, id: NodeID) -> AstGuard<T> {
        let ast = self.asts.get(id.ast().0).unwrap().borrow();
        AstGuard(id, ast)
    }

    pub fn get(&self, id: AstID) -> &RefCell<Ast<T>> {
        self.asts.get(id.0).unwrap()
    }

    pub fn iter(&self) -> impl Iterator<Item = &RefCell<Ast<T>>> + '_ {
        self.asts.iter()
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut RefCell<Ast<T>>> + '_ {
        self.asts.iter_mut()
    }

    pub fn named(&self) -> impl Iterator<Item = (&PathKey, &RefCell<Ast<T>>)> + '_ {
        self.names.iter().map(|(name, i)| (name, &self.asts[i.0]))
    }

    pub fn guarantee_state<D: Debug>(self) -> AstCollection<D> {
        unsafe { mem::transmute(self) }
    }

    pub fn add(&mut self, path: Path, ast: Ast<T>) {
        let id = ast.id();
        self.names.insert(path.key(), id);
        *self.asts[id.0].borrow_mut() = ast;
    }
}

#[derive(Default, Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct NodeID(usize, AstID);

impl NodeID {
    pub fn new(id: usize, ast_id: AstID) -> Self {
        NodeID(id, ast_id)
    }

    pub fn zero(ast_id: AstID) -> Self {
        NodeID(0, ast_id)
    }

    pub fn ast(&self) -> AstID {
        self.1
    }

    fn index(&self) -> usize {
        self.0
    }
}

#[derive(Debug, Copy, Clone)]
pub struct ProcedureDeclarationNode(NodeID);

impl Deref for ProcedureDeclarationNode {
    type Target = NodeID;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl ProcedureDeclarationNode {
    pub fn try_new(id: NodeID, ast: &Ast) -> Option<Self> {
        match ast.get_node(id).body {
            NodeBody::ProcedureDeclaration(_) => Some(Self(id)),
            _ => None,
        }
    }

    pub fn id(&self) -> NodeID {
        self.0
    }

    pub fn args<'a>(&'_ self, ast: &'a Ast) -> &'a [NodeID] {
        match &ast.get_node(self.id()).body {
            NodeBody::ProcedureDeclaration(NBProcedureDeclaration { args, .. }) => &args,
            _ => unreachable!(),
        }
    }

    pub fn returns(&self, ast: &Ast) -> Option<NodeID> {
        match ast.get_node(self.id()).body {
            NodeBody::ProcedureDeclaration(NBProcedureDeclaration { returns, .. }) => returns,
            _ => unreachable!(),
        }
    }

    pub fn body(&self, ast: &Ast) -> NodeID {
        match ast.get_node(self.id()).body {
            NodeBody::ProcedureDeclaration(NBProcedureDeclaration { body, .. }) => body,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Node<T = state::StateAny>
where
    T: Debug,
{
    id: NodeID,
    pub tokens: Vec<Token>,
    tp: Option<InferredType>,
    pub body: NodeBody<T>,
    pub parent_id: Option<NodeID>,
    pub referenced_by: HashSet<NodeReference>,
    pub references: HashSet<NodeReference>,
    pub reference_types: Option<HashSet<SideEffect>>,
    _tp: PhantomData<fn() -> T>,
}

impl<T> Node<T>
where
    T: TypesInferred,
{
    pub fn tp(&self) -> &NodeType {
        match &self.tp {
            Some(tp) => &tp.tp,
            None => unreachable!(),
        }
    }

    pub fn tp_source(&self) -> NodeTypeSource {
        match &self.tp {
            Some(tp) => tp.source,
            None => unreachable!(),
        }
    }
}

impl<T> Node<T>
where
    T: Debug,
{
    pub fn id(&self) -> NodeID {
        self.id
    }

    pub fn maybe_inferred_tp(&self) -> Option<&InferredType> {
        self.tp.as_ref()
    }

    pub fn maybe_tp(&self) -> Option<&NodeType> {
        match &self.tp {
            Some(tp) => Some(&tp.tp),
            None => None,
        }
    }

    pub fn maybe_tp_source(&self) -> Option<NodeTypeSource> {
        match &self.tp {
            Some(tp) => Some(tp.source),
            None => None,
        }
    }

    pub fn infer_type(&mut self, itp: InferredType) {
        self.tp = Some(itp);
    }

    pub fn is_closure_boundary(&self) -> bool {
        match self.body {
            NodeBody::ProcedureDeclaration(NBProcedureDeclaration { .. }) => true,
            _ => false,
        }
    }

    pub fn is_dead(&self) -> bool {
        match &self.reference_types {
            Some(references) => references.is_empty(),
            None => false,
        }
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
    T: TypesInferred,
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

impl fmt::Display for NodeType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use NodeType::*;
        match self {
            VarArg { args } => {
                write!(f, "{}...", &**args)
            }
            Any => write!(f, "any"),
            NotYetImplemented => write!(f, "[NOT YET IMPLEMENTED]"),
            Void => write!(f, "void"),
            Int => write!(f, "int"),
            Float => write!(f, "float"),
            Bool => write!(f, "bool"),
            String => write!(f, "string"),
            Fn { args, returns } => {
                write!(f, "Fn(")?;
                for (i, arg) in args.iter().enumerate() {
                    write!(f, "{}", arg)?;
                    if i != args.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                match &**returns {
                    NodeType::Void => (),
                    returns => write!(f, " -> {}", returns)?,
                }
                write!(f, ")")
            }
            NewType { tp } => write!(f, "{}", tp),
            Type { ident, content } => write!(f, "{} -> type {}", ident, content),
            Unknown { ident } => write!(f, "UNKNOWN({})", ident),
            Struct { fields } => {
                write!(f, "{{ ")?;
                for (i, (k, v)) in fields.iter().enumerate() {
                    write!(f, "{}: {}", k, v)?;
                    if i != fields.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, " }}")
            }
        }
    }
}

#[derive(Debug)]
pub enum PartialNodeValue<T = StateAny> {
    Linked(NodeValue<T>),
    Unlinked(String),
    _TP(PhantomData<fn() -> T>),
}

impl<T> Clone for PartialNodeValue<T> {
    fn clone(&self) -> Self {
        use PartialNodeValue::*;
        match self {
            Linked(v) => Linked((*v).clone()),
            Unlinked(v) => Unlinked(v.clone()),
            _ => unreachable!(),
        }
    }

    fn clone_from(&mut self, source: &Self) {
        *self = source.clone()
    }
}

impl<'a, T> Into<&'a NodeValue<T>> for &'a PartialNodeValue<T>
where
    T: Linked,
{
    fn into(self) -> &'a NodeValue<T> {
        match self {
            PartialNodeValue::Linked(v) => v,
            _ => unreachable!(),
        }
    }
}

impl<T> Into<NodeValue<T>> for PartialNodeValue<T>
where
    T: Linked,
{
    fn into(self) -> NodeValue<T> {
        match self {
            PartialNodeValue::Linked(v) => v,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug)]
pub enum NodeValue<T = StateAny> {
    Int(isize),
    Float(f64),
    Bool(bool),
    String(String),
    RuntimeFn(usize),
    Struct(Vec<(String, PartialNodeValue<T>)>),
}

impl<T> Clone for NodeValue<T> {
    fn clone(&self) -> Self {
        use NodeValue::*;
        match self {
            Int(v) => Int(*v),
            Float(v) => Float(*v),
            Bool(v) => Bool(*v),
            RuntimeFn(v) => RuntimeFn(*v),
            String(v) => String(v.clone()),
            Struct(v) => Struct(v.clone()),
        }
    }

    fn clone_from(&mut self, source: &Self) {
        *self = source.clone()
    }
}

impl<T> Into<PartialNodeValue<T>> for NodeValue<T> {
    fn into(self) -> PartialNodeValue<T> {
        PartialNodeValue::Linked(self)
    }
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
    use std::fmt::Debug;

    pub trait _Any {}
    pub trait _Linked {}
    pub trait _TypesInferred {}
    pub trait _TypesChecked {}

    pub trait Any = _Any + Debug;
    pub trait Linked = _Linked + Debug;
    pub trait TypesInferred = _TypesInferred + Debug;
    pub trait TypesChecked = _TypesChecked + Debug;

    #[derive(Debug, Copy, Clone)]
    pub struct StateAny {}
    impl _Any for StateAny {}

    #[derive(Debug, Copy, Clone)]
    pub struct StateLinked {}
    impl _Any for StateLinked {}
    impl _Linked for StateLinked {}

    #[derive(Debug, Copy, Clone)]
    pub struct StateTypesInferred {}
    impl _Any for StateTypesInferred {}
    impl _Linked for StateTypesInferred {}
    impl _TypesInferred for StateTypesInferred {}

    #[derive(Debug, Copy, Clone)]
    pub struct StateTypesChecked {}
    impl _Any for StateTypesChecked {}
    impl _Linked for StateTypesChecked {}
    impl _TypesInferred for StateTypesChecked {}
    impl _TypesChecked for StateTypesChecked {}
}

pub use state::{Any, Linked, TypesChecked, TypesInferred};
pub use state::{StateAny, StateLinked, StateTypesChecked, StateTypesInferred};

pub struct Ast<T = state::StateAny>
where
    T: Debug,
{
    pub file_name: String,
    ast_id: AstID,
    nodes: Vec<Node<T>>,
    root: NodeID,
    _tp: PhantomData<fn() -> T>,
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
    pub fn id(&self) -> AstID {
        self.ast_id
    }

    pub fn nodes_after(&self, node_id: NodeID) -> Vec<NodeID> {
        match self.nodes().last() {
            Some(last) => {
                let ast_id = node_id.ast();
                let start = node_id.index();
                let end = last.id().index();
                (start..=end)
                    .into_iter()
                    .map(|i| NodeID::new(i, ast_id))
                    .collect()
            }
            None => vec![node_id],
        }
    }

    pub fn exports(&self) -> HashMap<String, (NodeID, bool)> {
        let root_id = self.root();
        let root = self.get_node(root_id);
        root.body
            .children()
            .filter_map(|id| {
                let child = self.get_node(*id);
                match &child.body {
                    NodeBody::StaticDeclaration { ident, .. }
                    | NodeBody::TypeDeclaration { ident, .. } => {
                        Some((ident.to_string(), (*id, true)))
                    }
                    NodeBody::ConstDeclaration { ident, .. }
                    | NodeBody::VariableDeclaration { ident, .. } => {
                        Some((ident.to_string(), (*id, false)))
                    }
                    _ => None,
                }
            })
            .collect()
    }

    pub fn single_error(&self, details: &str, row_details: &str, nodes: Vec<NodeID>) -> Err {
        // Ugly but it's an error so it's not the happy path!
        Err::single(details, row_details, nodes)
    }

    pub fn new(file_name: String, ast_id: AstID) -> Self {
        Self {
            file_name,
            ast_id,
            root: NodeID::zero(ast_id),
            nodes: Vec::new(),
            _tp: PhantomData::default(),
        }
    }

    pub fn nodes(&self) -> &[Node<T>] {
        &self.nodes
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
            println!("WARNING: reference missing between target {:?} and referer {:?} during reference removal", target_id, referencer_id);
        } else if !removed_referenced && removed_referencer {
            println!("WARNING: reference missing from target {:?} to referer {:?} during reference removal", target_id, referencer_id);
        } else if removed_referenced && !removed_referencer {
            println!("WARNING: removed reference missing to target {:?} from referer {:?} during reference removal", target_id, referencer_id);
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
            id: NodeID::new(self.nodes.len(), self.ast_id),
            parent_id,
            referenced_by: Default::default(),
            references: Default::default(),
            tp: None,
            tokens: vec![],
            body: NodeBody::Empty,
            reference_types: None,
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

        write!(f, " Node({},{}):", node.id.index(), node.id.ast().0)?;
        if node.has_closure_references() {
            write!(f, " closure ")?;
        }
        if let Some(InferredType { source, tp }) = &node.tp {
            if !node.has_closure_references() {
                write!(f, " ")?;
            }
            write!(f, "{{ {:?}, {} }} ", source, tp)?;
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
        assert_eq!(node_id.ast(), self.ast_id, "Accessing node from wrong ast");
        match self.nodes.get(node_id.0) {
            Some(node) => node,
            None => panic!("Could not find Node({}) in ast", node_id.0),
        }
    }

    pub fn get_node_and_children(&self, node_id: NodeID) -> Vec<NodeID> {
        let mut nodes = vec![node_id];
        for &child_id in self.get_node(node_id).body.children() {
            let mut children = self.get_node_and_children(child_id);
            nodes.append(&mut children);
        }
        nodes
    }

    pub fn get_node_type(&self, node_id: NodeID) -> Option<NodeType> {
        self.get_node(node_id).tp.clone().map(|t| t.tp)
    }

    pub fn get_node_mut(&mut self, node_id: NodeID) -> &mut Node<T> {
        match self.nodes.get_mut(node_id.0) {
            Some(node) => node,
            None => panic!("Could not find Node({}) in ast", node_id.0),
        }
    }

    pub fn closest_fn(&self, node_id: NodeID) -> Option<(NodeID, NodeReferenceLocation)> {
        self.closest(node_id, |node| {
            Ok(match node.body {
                NodeBody::ProcedureDeclaration(_) => Some(node.id),
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
                let ident = match &child.body {
                    VariableDeclaration { ident, .. }
                    | ConstDeclaration { ident, .. }
                    | StaticDeclaration { ident, .. }
                    | TypeDeclaration { ident, .. } => ident,
                    Import { module, path, .. } => path.last().unwrap_or(module),
                    _ => continue,
                };
                if ident == target_ident {
                    if let Some(closest_id) = closest_id {
                        return Err(Err::single(
                            "Multiple variable declarations with same name encountered",
                            "Variable declaration",
                            vec![closest_id, child_id],
                        ));
                    }
                    closest_id = Some(child_id);
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
