use super::Result;
use crate::ast::Err;
use crate::token::{ArithmeticOP, Token};
use std::collections::HashSet;
use std::fmt;
use std::fmt::Formatter;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct NodeID(usize);

impl NodeID {
    pub fn new(id: usize) -> Self {
        NodeID(id)
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
}

impl Node {
    pub fn is_closure_boundary(&self) -> bool {
        match self.body {
            NodeBody::ProcedureDeclaration(..) => true,
            _ => false,
        }
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
pub enum NodeReferenceType {
    AssignValue,
    ReceiveValue,
    GoTo,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
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
    VarArg(Box<NodeType>),
    Any,
    NotYetImplemented,
    Void,
    Int,
    Float,
    Bool,
    String,
    Fn(Vec<NodeType>, Box<NodeType>),
    Type(Box<NodeType>),
    Unknown(String),
    Struct(Vec<(String, NodeType)>),
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
    Op(ArithmeticOP, NodeID, NodeID),
    ProcedureDeclaration(Vec<NodeID>, Option<NodeType>, NodeID),
    PrefixOp(ArithmeticOP, NodeID),
    Block(Vec<NodeID>),
    If(NodeID, NodeID),
    Loop(NodeID),
    Expression(NodeID),
    Comment(String),
    Import(String, NodeID),

    VariableDeclaration(String, Option<NodeType>, Option<NodeID>),
    ConstDeclaration(String, Option<NodeType>, NodeID),
    StaticDeclaration(String, Option<NodeType>, NodeID),
    TypeDeclaration(String, NodeType, NodeID, Option<NodeValue>),
    VariableAssignment(NodeID, Option<Vec<String>>, NodeID),
    VariableValue(NodeID, Option<Vec<String>>),
    Return(NodeID, Option<NodeID>),
    Break(NodeID),
    Call(NodeID, Vec<NodeID>),

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
    VariableAssignment(String, Option<Vec<String>>, NodeID),
    Value(NodeValue),
    VariableValue(String, Option<Vec<String>>),
    Type(Box<NodeBody>, Option<NodeID>),
    Return(Option<NodeID>),
    Break,
    Call(String, Vec<NodeID>),
    ImportValue(String),
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
            root: NodeID(0),
            nodes: Vec::new(),
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
            tp: None,
            tokens: vec![],
            body: NodeBody::Empty,
        };
        let id = node.id;
        self.nodes.push(node);
        id
    }

    pub fn root(&self) -> NodeID {
        self.root
    }

    fn fmt_debug_node(&self, f: &mut Formatter<'_>, level: usize, node_id: NodeID) -> fmt::Result {
        let node = &self.nodes[node_id.0];
        let pad = " ".repeat(level * 2);
        let mut children = node.body.children().peekable();
        let location_text = if node.has_closure_references() {
            "closure"
        } else {
            ""
        };
        write!(
            f,
            "{}Node({}): {} {:?} = {:?}",
            pad, node.id.0, location_text, node.tp, node.body
        )?;
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
            write!(f, "{}]", pad)?;
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
                NodeBody::ProcedureDeclaration(..) => Some(node.id),
                _ => None,
            })
        })
        .unwrap()
    }

    pub fn closest_loop(&self, node_id: NodeID) -> Option<(NodeID, NodeReferenceLocation)> {
        self.closest(node_id, |node| {
            Ok(match node.body {
                NodeBody::Loop(..) => Some(node.id),
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
                    VariableDeclaration(ident, ..)
                    | ConstDeclaration(ident, ..)
                    | StaticDeclaration(ident, ..)
                    | TypeDeclaration(ident, ..)
                    | Import(ident, _) => {
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
                Ok(option) => {
                    if let Some(node_id) = option {
                        break Ok(Some((node_id, location)));
                    } else if let Some(parent_id) = node.parent_id {
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
            Op(_, lhs, rhs) => match self.index {
                0 => Some(lhs),
                1 => Some(rhs),
                _ => None,
            },
            If(cond, body) => match self.index {
                0 => Some(cond),
                1 => Some(body),
                _ => None,
            },
            ProcedureDeclaration(args, _, body) => {
                if self.index < args.len() {
                    args.get(self.index)
                } else if self.index == args.len() {
                    Some(body)
                } else {
                    None
                }
            }
            Block(children) => children.get(self.index),
            Call(_, args) => args.get(self.index),

            Return(_, value) | VariableDeclaration(.., value) => match self.index {
                0 => value.as_ref(),
                _ => None,
            },
            PrefixOp(_, value)
            | Loop(value)
            | Expression(value)
            | TypeDeclaration(_, _, value, ..)
            | ConstDeclaration(.., value)
            | StaticDeclaration(.., value)
            | VariableAssignment(_, _, value)
            | Import(_, value) => match self.index {
                0 => Some(value),
                _ => None,
            },
            VariableValue(_, _) | Comment(_) | Break(_) | ConstValue(_) | Empty => None,
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
            VariableAssignment(_, _, value) => match self.index {
                0 => Some(value),
                _ => None,
            },
            Return(value) => match self.index {
                0 => value.as_ref(),
                _ => None,
            },
            Type(_, children) => match self.index {
                0 => children.as_ref(),
                _ => None,
            },
            Call(_, args) => args.get(self.index),
            VariableValue(_, _) | Break | ImportValue(_) | Value(_) => None,
        };
        if option.is_some() {
            self.index += 1;
        }
        option
    }
}
