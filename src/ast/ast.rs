use super::{Error, Result};
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

    pub fn index(self) -> usize {
        self.0
    }
}

#[derive(Debug)]
pub struct Node {
    pub id: NodeID,
    pub parent_id: Option<NodeID>,
    pub referenced_by: HashSet<NodeReference>,
    pub tp: Option<InferredType>,
    pub tokens: Vec<Token>,
    pub body: NodeBody,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct NodeReference {
    pub id: NodeID,
    pub ref_tp: NodeReferenceType,
}

impl NodeReference {
    pub fn new(id: NodeID, ref_tp: NodeReferenceType) -> Self {
        Self { id, ref_tp }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum NodeReferenceType {
    AssignValue,
    ReceiveValue,
    GoTo,
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

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum NodeTypeSource {
    Usage,
    Value,
    Variable,
    Declared,
}

#[derive(Debug, Clone, PartialEq)]
pub enum NodeType {
    NotYetImplemented,
    Void,
    Int,
    Fn(Vec<NodeType>, Box<NodeType>),
}

#[derive(Debug)]
pub enum NodeValue {
    Int(isize),
}

#[derive(Debug)]
pub enum NodeBody {
    Empty,
    ConstValue(NodeValue),
    Op(ArithmeticOP, NodeID, NodeID),
    ProcedureDeclaration(Vec<NodeID>, Option<String>, NodeID),
    PrefixOp(ArithmeticOP, NodeID),
    Block(Vec<NodeID>),
    If(NodeID, NodeID),
    Loop(NodeID),
    Expression(NodeID),
    Comment(String),

    VariableDeclaration(String, Option<String>, Option<NodeID>),
    ConstDeclaration(String, Option<String>, NodeID),
    VariableAssignment(NodeID, NodeID),
    VariableValue(NodeID),
    Return(NodeID),
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

#[derive(Debug)]
pub enum UnlinkedNodeBody {
    VariableAssignment(String, NodeID),
    VariableValue(String),
    Return,
    Break,
    Call(String, Vec<NodeID>),
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
        self.fmt_debug_node(f, 0, self.root)
    }
}

impl Ast {
    pub fn new(root: NodeID, nodes: Vec<Node>) -> Self {
        Self { root, nodes }
    }

    pub fn root(&self) -> NodeID {
        self.root
    }

    fn fmt_debug_node(&self, f: &mut Formatter<'_>, level: usize, node_id: NodeID) -> fmt::Result {
        let node = &self.nodes[node_id.0];
        let pad = " ".repeat(level * 2);
        let mut children = node.body.children().peekable();
        write!(
            f,
            "{}Node({}): {:?} = {:?}",
            pad, node.id.0, node.tp, node.body
        )?;
        if children.peek().is_some() {
            write!(f, " [\n")?;
            for &child in children {
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

    pub fn closest_fn(&self, node_id: NodeID) -> Result {
        let closest = self.closest(node_id, &|node| match node.body {
            NodeBody::ProcedureDeclaration(..) => Some(node.id),
            _ => None,
        });
        match closest {
            Some(id) => Ok(id),
            _ => Err(Error::new(&format!(
                "No function ancestor found starting at {:?}",
                node_id
            ))),
        }
    }

    pub fn closest_loop(&self, node_id: NodeID) -> Result {
        let closest = self.closest(node_id, &|node| match node.body {
            NodeBody::Loop(..) => Some(node.id),
            _ => None,
        });
        match closest {
            Some(id) => Ok(id),
            _ => Err(Error::new(&format!(
                "No loop ancestor found starting at {:?}",
                node_id
            ))),
        }
    }

    pub fn closest_variable(&self, node_id: NodeID, target_ident: &str) -> Result<NodeID> {
        use NodeBody::*;
        let closest = self.closest(node_id, &|node| {
            for &child_id in node.body.children() {
                let child = self.get_node(child_id);
                match &child.body {
                    VariableDeclaration(ident, ..) | ConstDeclaration(ident, ..) => {
                        if ident == target_ident {
                            return Some(child_id);
                        }
                    }
                    _ => (),
                }
            }
            None
        });
        match closest {
            Some(id) => Ok(id),
            _ => Err(Error::new(&format!(
                "Failed to find variable in scope starting at {:?}",
                node_id
            ))),
        }
    }

    pub fn closest(
        &self,
        mut node_id: NodeID,
        test: &dyn Fn(&Node) -> Option<NodeID>,
    ) -> Option<NodeID> {
        loop {
            let node = self.get_node(node_id);
            let result = test(node);
            if result.is_some() {
                break result;
            } else if let Some(id) = node.parent_id {
                node_id = id;
            } else {
                break None;
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
            ProcedureDeclaration(args, _, body) => {
                if self.index < args.len() {
                    args.get(self.index)
                } else if self.index == args.len() {
                    Some(body)
                } else {
                    None
                }
            }
            PrefixOp(_, op) => match self.index {
                0 => Some(op),
                _ => None,
            },
            Block(children) => children.get(self.index),
            If(cond, body) => match self.index {
                0 => Some(cond),
                1 => Some(body),
                _ => None,
            },
            Loop(body) => match self.index {
                0 => Some(body),
                _ => None,
            },
            Expression(expr) => match self.index {
                0 => Some(expr),
                _ => None,
            },
            VariableDeclaration(.., value) => match self.index {
                0 => value.as_ref(),
                _ => None,
            },
            ConstDeclaration(.., value) => match self.index {
                0 => Some(value),
                _ => None,
            },
            VariableAssignment(_, value) => match self.index {
                0 => Some(value),
                _ => None,
            },
            Call(_, args) => args.get(self.index),

            VariableValue(_) | Comment(_) | Return(_) | Break(_) | ConstValue(_) | Empty => None,
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
            VariableAssignment(_, value) => match self.index {
                0 => Some(value),
                _ => None,
            },
            Call(_, args) => args.get(self.index),
            VariableValue(_) | Return | Break => None,
        };
        if option.is_some() {
            self.index += 1;
        }
        option
    }
}
