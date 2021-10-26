use super::Result;
use crate::ast::Err;
use crate::token::{ArithmeticOP, Token};
use std::collections::HashSet;
use std::fmt;
use std::fmt::{Debug, Formatter};

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

#[derive(Debug, Clone)]
pub struct Node {
    pub id: NodeID,
    pub tokens: Vec<Token>,
    pub tp: Option<InferredType>,
    pub body: NodeBody,
    pub parent_id: Option<NodeID>,
    pub referenced_by: HashSet<NodeReference>,
    pub references: HashSet<NodeReference>,
    pub is_referenced: bool,
}

impl Node {
    pub fn is_closure_boundary(&self) -> bool {
        match self.body {
            NodeBody::ProcedureDeclaration { .. } => true,
            _ => false,
        }
    }

    pub fn is_dead(&self) -> bool {
        !self.is_referenced
    }

    pub fn has_closure_references(&self) -> bool {
        self.referenced_by
            .iter()
            .any(|ref_by| ref_by.ref_loc == NodeReferenceLocation::Closure)
    }

    pub fn child_tokens(&self, ast: &Ast) -> Vec<Token> {
        let mut tokens = Vec::new();
        for child_id in self.body.children() {
            let child = ast.get_node(*child_id);
            tokens.append(&mut child.tokens.clone());
            tokens.append(&mut child.child_tokens(ast))
        }
        tokens
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct NodeReference {
    pub id: NodeID,
    pub ref_tp: NodeReferenceType,
    pub ref_loc: NodeReferenceLocation,
    pub ref_effect: SideEffectSet,
}

impl NodeReference {
    pub fn new(id: NodeID, ref_tp: NodeReferenceType, ref_loc: NodeReferenceLocation) -> Self {
        Self {
            id,
            ref_tp,
            ref_loc,
            ref_effect: SideEffectSet::new(),
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum SideEffect {
    Read,
    Write,
    Execute,
}

impl Into<SideEffectSet> for SideEffect {
    fn into(self) -> SideEffectSet {
        SideEffectSet(self.bit())
    }
}

impl SideEffect {
    fn bit(&self) -> usize {
        match self {
            SideEffect::Read => 0x01,
            SideEffect::Write => 0x02,
            SideEffect::Execute => 0x04,
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct SideEffectSet(usize);

impl Debug for SideEffectSet {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.empty() {
            write!(f, "-")?;
        }
        if self.is(SideEffect::Read) {
            write!(f, "R")?;
        }
        if self.is(SideEffect::Write) {
            write!(f, "W")?;
        }
        if self.is(SideEffect::Execute) {
            write!(f, "X")?;
        }
        Ok(())
    }
}

impl SideEffectSet {
    pub fn new() -> SideEffectSet {
        SideEffectSet(0)
    }

    pub fn empty(&self) -> bool {
        self.0 == 0
    }

    pub fn is(&self, side_effect: SideEffect) -> bool {
        (self.0 & side_effect.bit()) > 0
    }

    pub fn or(&mut self, set: SideEffectSet) {
        self.0 |= set.0
    }

    pub fn add(&mut self, side_effect: SideEffect) {
        self.0 |= side_effect.bit()
    }

    pub fn remove(&mut self, side_effect: SideEffect) {
        self.0 ^= side_effect.bit()
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
    VarArg { args: Box<NodeType> },
    Any,
    NotYetImplemented,
    Void,
    Int,
    Float,
    Bool,
    String,
    Fn { args: Vec<NodeType>, returns: Box<NodeType> },
    Type { tp: Box<NodeType> },
    Unknown { ident: String },
    Struct { fields: Vec<(String, NodeType)> },
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
pub enum NodeBody {
    Empty,
    ConstValue(NodeValue),
    Op { op: ArithmeticOP, lhs: NodeID, rhs: NodeID },
    ProcedureDeclaration { args: Vec<NodeID>, returns: Option<NodeType>, body: NodeID },
    PrefixOp { op: ArithmeticOP, rhs: NodeID },
    Block { body: Vec<NodeID> },
    If { condition: NodeID, body: NodeID },
    Loop { body: NodeID },
    Expression(NodeID),
    Comment(String),
    Import { ident: String, expr: NodeID },

    VariableDeclaration { ident: String, tp: Option<NodeType>, expr: Option<NodeID> },
    ConstDeclaration { ident: String, tp: Option<NodeType>, expr: NodeID },
    StaticDeclaration { ident: String, tp: Option<NodeType>, expr: NodeID },
    TypeDeclaration { ident: String, tp: NodeType, constructor: NodeID, default_value: Option<NodeValue> },
    VariableAssignment { variable: NodeID, path: Option<Vec<String>>, expr: NodeID },
    VariableValue { variable: NodeID, path: Option<Vec<String>> },
    Return { func: NodeID, expr: Option<NodeID> },
    Break { r#loop: NodeID },
    Call { func: NodeID, args: Vec<NodeID> },

    Unlinked(UnlinkedNodeBody),
}

impl NodeBody {
    pub fn children(&self) -> NodeBodyIterator {
        NodeBodyIterator {
            index: 0,
            body: self,
            unlinked: None,
        }
    }
}

#[derive(Debug, Clone)]
pub enum UnlinkedNodeBody {
    VariableAssignment { ident: String, path: Option<Vec<String>>, expr: NodeID },
    Value(NodeValue),
    VariableValue { ident: String, path: Option<Vec<String>> },
    Type { def: Box<NodeBody>, expr: Option<NodeID> },
    Return { expr: Option<NodeID> },
    Break,
    Call { ident: String, args: Vec<NodeID> },
    ImportValue { ident: String },
}

impl UnlinkedNodeBody {
    pub fn children(&self) -> UnlinkedNodeBodyIterator {
        UnlinkedNodeBodyIterator {
            index: 0,
            body: self,
        }
    }
}

pub struct Ast {
    nodes: Vec<Node>,
    root: NodeID,
}

impl fmt::Debug for Ast {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.fmt_debug_node(f, 0, self.root)?;
        write!(f, "\n")
    }
}

impl Ast {
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
        }
    }

    pub fn add_ref(
        &mut self,
        target: (NodeID, NodeReferenceType),
        referencer: (NodeID, NodeReferenceType),
        loc: NodeReferenceLocation,
    ) {
        let referenced_by = NodeReference::new(referencer.0, referencer.1, loc);
        let inserted_referenced = self.get_node_mut(target.0)
            .referenced_by
            .insert(referenced_by);

        let references = NodeReference::new(target.0, target.1, loc);
        let inserted_references = self.get_node_mut(referencer.0)
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
        let removed_referenced = self.get_node_mut(target_id)
            .referenced_by
            .remove(&referenced_by);

        let references = NodeReference::new(target_id, tp, loc);
        let removed_referencer = self.get_node_mut(referencer_id)
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
            is_referenced: false,
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


        let prefix = if node.is_referenced { " " } else { "-" };
        write!(f, "{}", prefix)?;

        let pad_len = 2 + level * 2;
        let pad_start = " ".repeat(std::cmp::max(0, pad_len as isize - prefix.len() as isize) as usize);
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

    pub fn get_node(&self, node_id: NodeID) -> &Node {
        match self.nodes.get(node_id.0) {
            Some(node) => node,
            None => panic!("Could not find Node({}) in ast", node_id.0),
        }
    }

    pub fn get_node_mut(&mut self, node_id: NodeID) -> &mut Node {
        match self.nodes.get_mut(node_id.0) {
            Some(node) => node,
            None => panic!("Could not find Node({}) in ast", node_id.0),
        }
    }

    pub fn closest_fn(&self, node_id: NodeID) -> Option<(NodeID, NodeReferenceLocation)> {
        self.closest(node_id, |node| {
            Ok(match node.body {
                NodeBody::ProcedureDeclaration { .. } => Some(node.id),
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
        use NodeBody::*;
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
            F: Fn(&Node) -> Result<Option<NodeID>>,
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
}

pub struct NodeBodyIterator<'a> {
    index: usize,
    body: &'a NodeBody,
    unlinked: Option<UnlinkedNodeBodyIterator<'a>>,
}

impl<'a> Iterator for NodeBodyIterator<'a> {
    type Item = &'a NodeID;

    fn next(&mut self) -> Option<&'a NodeID> {
        use NodeBody::*;
        let option = match self.body {
            Op { lhs, rhs, .. } => match self.index {
                0 => Some(lhs),
                1 => Some(rhs),
                _ => None,
            },
            If { condition, body } => match self.index {
                0 => Some(condition),
                1 => Some(body),
                _ => None,
            },
            ProcedureDeclaration { args, body, .. } => {
                if self.index < args.len() {
                    args.get(self.index)
                } else if self.index == args.len() {
                    Some(body)
                } else {
                    None
                }
            }
            Block { body } => body.get(self.index),
            Call { args, .. } => args.get(self.index),

            Return { expr, .. } | VariableDeclaration { expr, .. } => match self.index {
                0 => expr.as_ref(),
                _ => None,
            },
            PrefixOp { rhs: value, .. }
            | Loop { body: value }
            | Expression(value)
            | TypeDeclaration { constructor: value, .. }
            | ConstDeclaration { expr: value, .. }
            | StaticDeclaration { expr: value, .. }
            | VariableAssignment { expr: value, .. }
            | Import { expr: value, .. } => match self.index {
                0 => Some(value),
                _ => None,
            },
            VariableValue { .. } | Comment { .. } | Break { .. } | ConstValue { .. } | Empty => None,
            Unlinked(body) => {
                if let None = self.unlinked {
                    self.unlinked = Some(body.children());
                }
                match &mut self.unlinked {
                    Some(iter) => iter.next(),
                    None => unreachable!(),
                }
            }
        };
        if option.is_some() {
            self.index += 1;
        }
        option
    }
}

pub struct UnlinkedNodeBodyIterator<'a> {
    index: usize,
    body: &'a UnlinkedNodeBody,
}

impl<'a> Iterator for UnlinkedNodeBodyIterator<'a> {
    type Item = &'a NodeID;

    fn next(&mut self) -> Option<&'a NodeID> {
        use UnlinkedNodeBody::*;
        let option = match self.body {
            VariableAssignment { expr, .. } => match self.index {
                0 => Some(expr),
                _ => None,
            },
            Return { expr } => match self.index {
                0 => expr.as_ref(),
                _ => None,
            },
            Type { expr, .. } => match self.index {
                0 => expr.as_ref(),
                _ => None,
            },
            Call { args, .. } => args.get(self.index),
            VariableValue { .. } | Break | ImportValue { .. } | Value { .. } => None,
        };
        if option.is_some() {
            self.index += 1;
        }
        option
    }
}
