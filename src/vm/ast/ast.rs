use super::Result;
use crate::vm::ast::{
    Err, IsLinked, IsTypesInferred, IsValid, LNBTypeDeclaration, LinkedNodeBody, Valid,
};
use crate::vm::ast::{NBProcedureDeclaration, PartialNodeBody};
use crate::vm::token::{Token, TokenSourceInfo};
use std::collections::{HashMap, HashSet};
use std::fmt::{Debug, Formatter};
use std::marker::PhantomData;
use std::ops::{Deref, DerefMut};
use std::sync::{RwLock, RwLockReadGuard, RwLockWriteGuard};
use std::time::Duration;
use std::{fmt, mem};

pub struct DebugSymbols {
    pub file_info: Vec<Vec<(TokenSourceInfo, TokenSourceInfo)>>,
    pub files: Vec<Path>,
}

impl DebugSymbols {
    pub fn get_file_line(&self, node_id: NodeID) -> String {
        let file = self.files.get(node_id.ast().index());
        let line = self
            .file_info
            .get(node_id.ast().index())
            .map(|ast| ast[node_id.index()])
            .map(|(start, _)| start.line);
        match (file, line) {
            (Some(file), Some(line)) => format!("{}.bc:{}", file.as_ref().as_ref().join("/"), line),
            (Some(file), None) => format!("{}.bc", file.as_ref().as_ref().join("/")),
            (None, Some(line)) => format!("{}", line),
            (None, None) => format!("Unknown"),
        }
    }
}

#[derive(Default, Debug, Copy, Clone, PartialEq, Eq, Ord, PartialOrd, Hash)]
pub struct AstBranchID(usize);

impl AstBranchID {
    pub fn new(id: usize) -> Self {
        Self(id)
    }

    pub fn index(&self) -> usize {
        self.0
    }
}

#[derive(Default, Debug)]
pub struct AstCollectionDebugInfo {
    pub build_duration: Duration,
    pub link_duration: Duration,
    pub treeshake_duration: Duration,
    pub type_inference_duration: Duration,
    pub type_checker_duration: Duration,
    pub line_count: usize,
    pub file_count: usize,
}

pub struct Ast<T = Valid>
where
    T: Debug,
{
    asts: Vec<RwLock<AstBranch<T>>>,
    names: HashMap<Path, AstBranchID>,
    pub debug_info: AstCollectionDebugInfo,
}

impl<T> Debug for Ast<T>
where
    T: Debug,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "AstCollection {{\n")?;
        for (i, (name, ast)) in self.paths().enumerate() {
            write!(f, "  {:?}:\n", name)?;
            if ast.nodes.len() < 100 {
                write!(f, "{:?}", ast)?;
            } else {
                write!(f, "{{ ... }}")?;
            }
            if i + 1 < self.asts.len() {
                write!(f, "\n")?;
            }
        }
        write!(f, "}}")
    }
}

pub struct AstGuard<'a, T>(NodeID, RwLockReadGuard<'a, AstBranch<T>>)
where
    T: IsValid;

impl<'a, T> Deref for AstGuard<'a, T>
where
    T: IsValid,
{
    type Target = Node<T>;

    fn deref(&self) -> &Node<T> {
        &self.1.get_node(self.0)
    }
}

pub struct AstGuardMut<'a, T>(NodeID, RwLockWriteGuard<'a, AstBranch<T>>)
where
    T: IsValid;

impl<'a, T> Deref for AstGuardMut<'a, T>
where
    T: IsValid,
{
    type Target = Node<T>;

    fn deref(&self) -> &Node<T> {
        &self.1.get_node(self.0)
    }
}

impl<'a, T> DerefMut for AstGuardMut<'a, T>
where
    T: IsValid,
{
    fn deref_mut(&mut self) -> &mut Node<T> {
        self.1.get_node_mut(self.0)
    }
}

impl<T> Ast<T>
where
    T: IsValid,
{
    pub fn new() -> Self {
        Self {
            asts: vec![],
            names: Default::default(),
            debug_info: Default::default(),
        }
    }

    pub fn reserve(&mut self) -> AstBranchID {
        let id = AstBranchID::new(self.asts.len());
        self.asts.push(RwLock::new(AstBranch::new(
            Path::single("RESERVED".to_string()),
            id,
            0,
        )));
        id
    }

    pub fn get_node(&self, id: NodeID) -> AstGuard<T> {
        let ast = self.read_ast(id.ast());
        AstGuard(id, ast)
    }

    pub unsafe fn get_node_mut(&self, id: NodeID) -> AstGuardMut<T> {
        let ast = unsafe {
            // Safety: Caller ensure that there is no other mutable references
            self.write_ast(id.ast())
        };
        AstGuardMut(id, ast)
    }

    pub unsafe fn write_ast(&self, id: AstBranchID) -> RwLockWriteGuard<AstBranch<T>> {
        self.asts.get(id.index()).unwrap().write().unwrap()
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = RwLockWriteGuard<AstBranch<T>>> + '_ {
        self.asts.iter().map(|ast| ast.write().unwrap())
    }

    pub fn guarantee_state<D: Debug>(self) -> Ast<D> {
        unsafe {
            // Safety: safe since D is only used a phantom data
            // and will therefore always be zero sized.
            // Hopefully the memory representation stays stable...
            mem::transmute(self)
        }
    }

    pub fn add(&mut self, path: Path, ast: AstBranch<T>) {
        let id = ast.id();
        self.names.insert(path, id);
        *self.asts[id.index()].write().unwrap() = ast;
    }

    pub fn add_ref(
        &mut self,
        target: (NodeID, NodeReferenceType),
        referencer: (NodeID, NodeReferenceType),
        loc: NodeReferenceLocation,
    ) {
        let referenced_by = NodeReference::new(referencer.0, referencer.1, loc);
        let mut ast = unsafe {
            // Safety: Caller ensure that there is no other mutable references on this thread
            self.write_ast(target.0.ast())
        };
        let inserted_referenced = ast
            .get_node_mut(target.0)
            .referenced_by
            .insert(referenced_by);
        assert!(inserted_referenced);
        drop(ast);

        let references = NodeReference::new(target.0, target.1, loc);
        let mut ast = unsafe {
            // Safety: Caller ensure that there is no other mutable references on this thread
            self.write_ast(referencer.0.ast())
        };
        let inserted_references = ast.get_node_mut(referencer.0).references.insert(references);
        assert!(inserted_references);
    }
}
impl<T> Ast<T>
where
    T: Debug,
{
    pub fn clear(&mut self) {
        self.asts.clear();
        self.names.clear();
        self.debug_info = Default::default();
    }

    pub fn root(&self) -> NodeID {
        self.asts[0].read().unwrap().root()
    }

    pub fn read_ast(&self, id: AstBranchID) -> RwLockReadGuard<AstBranch<T>> {
        self.asts.get(id.index()).unwrap().read().unwrap()
    }

    pub fn iter(&self) -> impl Iterator<Item = RwLockReadGuard<AstBranch<T>>> + '_ {
        self.asts.iter().map(|ast| ast.read().unwrap())
    }

    pub fn ids(&self) -> impl Iterator<Item = AstBranchID> + '_ {
        self.iter().map(|ast| ast.id())
    }

    pub fn paths(&self) -> impl Iterator<Item = (&Path, RwLockReadGuard<AstBranch<T>>)> + '_ {
        self.names
            .iter()
            .map(|(path, id)| (path, self.asts[id.index()].read().unwrap()))
    }

    pub fn len(&self) -> usize {
        self.asts.len()
    }
}

#[derive(Default, Copy, Clone, PartialEq, Eq, Hash)]
pub struct NodeID(usize, AstBranchID);

impl Debug for NodeID {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "Node({},{})", self.0, self.1 .0)
    }
}

impl NodeID {
    pub fn new(id: usize, ast_id: AstBranchID) -> Self {
        NodeID(id, ast_id)
    }

    pub fn zero(ast_id: AstBranchID) -> Self {
        NodeID(0, ast_id)
    }

    pub fn ast(&self) -> AstBranchID {
        self.1
    }

    pub fn index(&self) -> usize {
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
    pub fn try_new<T>(id: NodeID, branch: &AstBranch<T>) -> Option<Self>
    where
        T: IsValid,
    {
        match branch.get_node(id).body {
            ast::PartialNodeBody::Linked(ast::LinkedNodeBody::ProcedureDeclaration(_)) => {
                Some(Self(id))
            }
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Node<T = Valid>
where
    T: Debug,
{
    id: NodeID,
    pub tokens: Vec<Token>,
    tp: Option<InferredType>,
    pub body: PartialNodeBody<T>,
    pub parent_id: Option<NodeID>,
    pub referenced_by: HashSet<NodeReference>,
    pub references: HashSet<NodeReference>,
    pub reference_types: Option<HashSet<SideEffect>>,
    _tp: PhantomData<fn() -> T>,
}

impl<T> Node<T>
where
    T: IsTypesInferred,
{
    pub fn tp(&self) -> &NodeType {
        match &self.tp {
            Some(tp) => &tp.tp,
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

    pub fn is_dead(&self) -> bool {
        match &self.reference_types {
            Some(references) => references.is_empty(),
            None => false,
        }
    }

    pub fn is_closure_boundary(&self) -> bool {
        use LinkedNodeBody::*;
        use PartialNodeBody::*;
        match self.body {
            Linked(ProcedureDeclaration(NBProcedureDeclaration { .. })) => true,
            _ => false,
        }
    }

    pub fn has_closure_references(&self) -> bool {
        self.referenced_by
            .iter()
            .any(|ref_by| ref_by.ref_loc == NodeReferenceLocation::Closure)
    }

    pub fn child_tokens(&self, ast: &AstBranch<T>) -> Vec<Token> {
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
    T: IsValid,
{
    pub fn set_inferred_tp(&mut self, itp: InferredType) {
        self.tp = Some(itp);
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

    pub fn insert(&mut self, item: SideEffect) {
        self.0.insert(item);
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum NodeReferenceType {
    ReadExternalValue,
    WriteExternalValue,
    ReadValue,
    WriteValue,
    ExecuteValue,
    GoTo,
    ControlFlow,
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
}

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub enum NodeTypeSource {
    Value,
    Declared,
}

#[derive(Debug, Clone, PartialEq)]
pub enum NodeType {
    VarArg {
        args: Box<NodeType>,
    },
    Any,
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
pub enum PartialNodeValue<T> {
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

impl<T> Deref for PartialNodeValue<T>
where
    T: IsLinked,
{
    type Target = ast::NodeValue<T>;

    fn deref(&self) -> &Self::Target {
        use PartialNodeValue::*;
        match &self {
            Linked(value) => value,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug)]
pub enum NodeValue<T> {
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

use crate::vm::ast;
use crate::vm::runtime::{AstExport, Namespace, NamespaceElement};
use crate::{Path, SubPath};

pub struct AstBranch<T = Valid>
where
    T: Debug,
{
    pub path: Path,
    pub line_count: usize,
    ast_id: AstBranchID,
    nodes: Vec<Node<T>>,
    root: NodeID,
    _tp: PhantomData<fn() -> T>,
}

impl<T> fmt::Debug for AstBranch<T>
where
    T: Debug,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.fmt_debug_node(f, 0, self.root)?;
        write!(f, "\n")
    }
}

impl<T> AstBranch<T>
where
    T: Debug,
{
    pub fn id(&self) -> AstBranchID {
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

    pub fn nodes(&self) -> &[Node<T>] {
        &self.nodes
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

        write!(f, " {:?}", node.id())?;
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
                    PartialNodeBody::Linked(LinkedNodeBody::Comment(..)) => continue,
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
            None => panic!("Could not find {:?} in ast", node_id),
        }
    }
}

impl<T> AstBranch<T>
where
    T: IsLinked,
{
    pub fn get_node_body(&self, node_id: NodeID) -> &LinkedNodeBody<T> {
        match &self.get_node(node_id).body {
            PartialNodeBody::Linked(b) => b,
            _ => unreachable!(),
        }
    }
}

impl<T> AstBranch<T>
where
    T: IsValid,
{
    pub fn exports(&self) -> Namespace {
        let mut namespace = Namespace::new();
        let root_id = self.root();
        let root = self.get_node(root_id);
        for id in root.body.children() {
            let child = self.get_node(*id);
            match &child.body {
                PartialNodeBody::Linked(body) => match body {
                    LinkedNodeBody::StaticDeclaration { ident, .. }
                    | LinkedNodeBody::TypeDeclaration(LNBTypeDeclaration { ident, .. }) => {
                        let name = &[ident.to_string()];
                        let path = SubPath::try_new(name).unwrap();
                        let export = AstExport {
                            node_id: *id,
                            is_static: true,
                        };
                        namespace.set(path, NamespaceElement::Export(export));
                    }
                    LinkedNodeBody::ConstDeclaration { ident, .. }
                    | LinkedNodeBody::VariableDeclaration { ident, .. } => {
                        let name = &[ident.to_string()];
                        let path = SubPath::try_new(name).unwrap();
                        let export = AstExport {
                            node_id: *id,
                            is_static: false,
                        };
                        namespace.set(path, NamespaceElement::Export(export));
                    }
                    _ => (),
                },
                _ => (),
            }
        }
        namespace
    }

    pub fn new(path: Path, ast_id: AstBranchID, size_hint: usize) -> Self {
        Self {
            path,
            ast_id,
            line_count: 0,
            root: NodeID::zero(ast_id),
            nodes: Vec::with_capacity(size_hint),
            _tp: PhantomData::default(),
        }
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
        assert!(inserted_referenced);

        let references = NodeReference::new(target.0, target.1, loc);
        let inserted_references = self
            .get_node_mut(referencer.0)
            .references
            .insert(references);
        assert!(inserted_references);
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
            body: PartialNodeBody::Empty,
            reference_types: None,
            _tp: PhantomData::default(),
        };
        let id = node.id;
        self.nodes.push(node);
        id
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
        assert_eq!(node_id.ast(), self.ast_id, "Accessing node from wrong ast");
        match self.nodes.get_mut(node_id.index()) {
            Some(node) => node,
            None => panic!("Could not find {:?} in ast", node_id),
        }
    }

    pub fn closest_fn(&self, node_id: NodeID) -> Option<(NodeID, NodeReferenceLocation, usize)> {
        self.closest(node_id, |node| {
            Ok(match node.body {
                PartialNodeBody::Linked(LinkedNodeBody::ProcedureDeclaration(_)) => Some(node.id),
                _ => None,
            })
        })
        .unwrap()
    }

    pub fn closest_loop(&self, node_id: NodeID) -> Option<(NodeID, NodeReferenceLocation, usize)> {
        self.closest(node_id, |node| {
            Ok(match node.body {
                PartialNodeBody::Linked(LinkedNodeBody::Loop { .. }) => Some(node.id),
                _ => None,
            })
        })
        .unwrap()
    }

    pub fn closest_variable(
        &self,
        node_id: NodeID,
        target_ident: &str,
    ) -> Result<Option<(NodeID, NodeReferenceLocation, usize)>> {
        use crate::vm::ast::LinkedNodeBody::*;
        self.closest(node_id, |node| {
            let mut closest_id = None;
            for &child_id in node.body.children() {
                let child = self.get_node(child_id);
                let ident = match &child.body {
                    PartialNodeBody::Linked(body) => match body {
                        VariableDeclaration { ident, .. }
                        | ConstDeclaration { ident, .. }
                        | StaticDeclaration { ident, .. }
                        | TypeDeclaration(LNBTypeDeclaration { ident, .. }) => ident,
                        Import { path, .. } => path.last(),
                        _ => continue,
                    },
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
    ) -> Result<Option<(NodeID, NodeReferenceLocation, usize)>>
    where
        F: Fn(&Node<T>) -> Result<Option<NodeID>>,
    {
        let mut depth = 0;
        let mut location = NodeReferenceLocation::Local;
        loop {
            let node = self.get_node(node_id);
            let result = test(node);
            match result {
                Ok(Some(node_id)) => {
                    break Ok(Some((node_id, location, depth)));
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
            depth += 1;
        }
    }

    pub fn partial_type<'a, 'b: 'a>(&'b self, node_id: NodeID) -> Option<(NodeID, &'a NodeType)> {
        let node = self.get_node(node_id);
        match &node.body {
            PartialNodeBody::Linked(body) => match body {
                LinkedNodeBody::TypeDeclaration(LNBTypeDeclaration { tp, .. })
                | LinkedNodeBody::TypeReference { tp } => self.partial_type(*tp),
                LinkedNodeBody::PartialType { tp, .. } => {
                    let tp = match tp.tp() {
                        NodeType::NewType { tp, .. } => tp,
                        tp => tp,
                    };
                    Some((node_id, tp))
                }
                _ => None,
            },
            _ => None,
        }
    }
}
