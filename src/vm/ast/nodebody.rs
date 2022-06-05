use crate::vm::ast;
use crate::vm::ast::{NodeID, PartialNodeValue, PartialType, ProcedureDeclarationNode};
use crate::Path;
use std::borrow::BorrowMut;
use std::collections::HashMap;
use std::fmt;
use std::fmt::Formatter;
use std::ops::{Deref, DerefMut};

#[derive(Debug, Clone)]
pub struct NBCall {
    pub func: NodeID,
    pub args: Vec<NodeID>,
}

#[derive(Debug, Clone)]
pub struct NBProcedureDeclaration {
    pub args: Vec<NodeID>,
    pub returns: Option<NodeID>,
    pub body: NodeID,
}

#[derive(Debug, Clone)]
pub enum PartialNodeBody<T> {
    Empty,
    Linked(LinkedNodeBody<T>),
    Unlinked(UnlinkedNodeBody<T>),
}

impl<T> PartialNodeBody<T> {
    pub fn children(&self) -> NodeBodyIterator<T> {
        NodeBodyIterator {
            body: self,
            linked: None,
            unlinked: None,
        }
    }
}

impl<T> Deref for PartialNodeBody<T>
where
    T: ast::IsLinked,
{
    type Target = LinkedNodeBody<T>;

    fn deref(&self) -> &Self::Target {
        use PartialNodeBody::*;
        match &self {
            Linked(body) => body,
            Unlinked(_) => unreachable!(),
            _ => unreachable!(),
        }
    }
}

impl<T> DerefMut for PartialNodeBody<T>
where
    T: ast::IsLinked,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        use PartialNodeBody::*;
        match self.borrow_mut() {
            Linked(body) => body,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct LNBTypeDeclaration<T> {
    pub ident: String,
    pub tp: NodeID,
    pub constructor: ProcedureDeclarationNode,
    pub methods: HashMap<String, NodeID>,
    pub default_value: Option<PartialNodeValue<T>>,
}

#[derive(Debug, Clone)]
pub enum LinkedNodeBody<T> {
    ConstValue {
        tp: Option<NodeID>,
        value: PartialNodeValue<T>,
    },
    TypeReference {
        tp: NodeID,
    },
    PartialType {
        tp: PartialType,
        parts: Vec<NodeID>,
    },
    Op {
        op: ArithmeticOP,
        lhs: NodeID,
        rhs: NodeID,
    },
    ProcedureDeclaration(NBProcedureDeclaration),
    PrefixOp {
        op: ArithmeticOP,
        rhs: NodeID,
    },
    Block {
        static_body: Vec<NodeID>,
        import_body: Vec<NodeID>,
        dynamic_body: Vec<NodeID>,
    },
    If {
        condition: NodeID,
        body: NodeID,
    },
    Loop {
        body: NodeID,
    },
    Expression(NodeID),
    Comment(String),
    Import {
        path: Path,
        expr: NodeID,
    },

    VariableDeclaration {
        ident: String,
        tp: Option<NodeID>,
        expr: Option<NodeID>,
    },
    ConstDeclaration {
        ident: String,
        tp: Option<NodeID>,
        expr: NodeID,
    },
    StaticDeclaration {
        ident: String,
        tp: Option<NodeID>,
        expr: NodeID,
    },
    Reference {
        node_id: NodeID,
    },
    TypeDeclaration(LNBTypeDeclaration<T>),
    ConstAssignment {
        ident: NodeID,
        path: Option<Vec<String>>,
        expr: NodeID,
    },
    VariableAssignment {
        variable: NodeID,
        path: Option<Vec<String>>,
        expr: NodeID,
    },
    VariableValue {
        variable: NodeID,
        path: Option<Vec<String>>,
    },
    Return {
        func: NodeID,
        expr: Option<NodeID>,
        automatic: bool,
    },
    Break {
        r#loop: NodeID,
    },
    Call(NBCall),
}

impl<T> LinkedNodeBody<T> {
    pub fn children(&self) -> LinkedNodeBodyIterator<T> {
        LinkedNodeBodyIterator {
            index: 0,
            body: self,
        }
    }
}

#[derive(Debug, Clone)]
pub enum UnlinkedNodeBody<T> {
    StaticAssignment {
        ident: String,
        path: Option<Vec<String>>,
        expr: NodeID,
    },
    VariableAssignment {
        ident: String,
        path: Option<Vec<String>>,
        expr: NodeID,
    },
    Value {
        tp: Option<NodeID>,
        value: PartialNodeValue<T>,
    },
    VariableValue {
        ident: String,
        path: Option<Vec<String>>,
    },
    Return {
        expr: Option<NodeID>,
        automatic: bool,
    },
    Break,
    Call {
        ident: String,
        path: Option<Vec<String>>,
        args: Vec<NodeID>,
    },
    ImportValue {
        is_relative: bool,
        path: Path,
    },
}

impl<T> UnlinkedNodeBody<T> {
    pub fn children(&self) -> UnlinkedNodeBodyIterator<T> {
        UnlinkedNodeBodyIterator {
            index: 0,
            body: self,
        }
    }
}

pub struct NodeBodyIterator<'a, T> {
    body: &'a PartialNodeBody<T>,
    linked: Option<LinkedNodeBodyIterator<'a, T>>,
    unlinked: Option<UnlinkedNodeBodyIterator<'a, T>>,
}

impl<'a, T> Iterator for NodeBodyIterator<'a, T> {
    type Item = &'a NodeID;

    fn next(&mut self) -> Option<Self::Item> {
        use PartialNodeBody::*;
        match &self.body {
            Linked(body) => {
                if let None = self.linked {
                    self.linked = Some(body.children());
                }
                match &mut self.linked {
                    Some(iter) => iter.next(),
                    None => unreachable!(),
                }
            }
            Unlinked(body) => {
                if let None = self.unlinked {
                    self.unlinked = Some(body.children());
                }
                match &mut self.unlinked {
                    Some(iter) => iter.next(),
                    None => unreachable!(),
                }
            }
            Empty => None,
        }
    }
}

pub struct LinkedNodeBodyIterator<'a, T> {
    index: usize,
    body: &'a LinkedNodeBody<T>,
}

impl<'a, T> Iterator for LinkedNodeBodyIterator<'a, T> {
    type Item = &'a NodeID;

    fn next(&mut self) -> Option<&'a NodeID> {
        use LinkedNodeBody::*;
        let option = match self.body {
            Reference { .. } => None,
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
            ProcedureDeclaration(NBProcedureDeclaration {
                args,
                body,
                returns,
            }) => {
                if self.index < args.len() {
                    args.get(self.index)
                } else if self.index == args.len() {
                    Some(body)
                } else if self.index == args.len() + 1 {
                    match returns {
                        Some(v) => Some(v),
                        None => None,
                    }
                } else {
                    None
                }
            }
            Block {
                static_body,
                import_body,
                dynamic_body,
            } => {
                if self.index < static_body.len() {
                    static_body.get(self.index)
                } else if self.index < static_body.len() + import_body.len() {
                    import_body.get(self.index - static_body.len())
                } else {
                    dynamic_body.get(self.index - static_body.len() - import_body.len())
                }
            }
            Call(NBCall { args, .. }) => args.get(self.index),
            PartialType { parts, .. } => parts.get(self.index),

            Return { expr, .. } => match self.index {
                0 => expr.as_ref(),
                _ => None,
            },
            PrefixOp { rhs: value, .. }
            | Loop { body: value }
            | Expression(value)
            | VariableAssignment { expr: value, .. }
            | ConstAssignment { expr: value, .. }
            | Import { expr: value, .. } => match self.index {
                0 => Some(value),
                _ => None,
            },
            TypeDeclaration(LNBTypeDeclaration {
                constructor,
                tp,
                methods,
                ..
            }) => match self.index {
                0 => Some(&**constructor),
                1 => Some(tp),
                i => methods.values().skip(i - 2).next(),
            },
            VariableDeclaration {
                expr: Option::None,
                tp,
                ..
            } => match self.index {
                0 => tp.as_ref(),
                _ => None,
            },
            VariableDeclaration {
                expr: Some(expr),
                tp,
                ..
            }
            | ConstDeclaration { expr, tp, .. }
            | StaticDeclaration { expr, tp, .. } => match self.index {
                0 => Some(expr),
                1 => tp.as_ref(),
                _ => None,
            },
            ConstValue { tp, .. } => match self.index {
                0 => tp.as_ref(),
                _ => None,
            },
            TypeReference { .. } | VariableValue { .. } | Comment { .. } | Break { .. } => None,
        };
        if option.is_some() {
            self.index += 1;
        }
        option
    }
}

pub struct UnlinkedNodeBodyIterator<'a, T> {
    index: usize,
    body: &'a UnlinkedNodeBody<T>,
}

impl<'a, T> Iterator for UnlinkedNodeBodyIterator<'a, T> {
    type Item = &'a NodeID;

    fn next(&mut self) -> Option<&'a NodeID> {
        use UnlinkedNodeBody::*;
        let option = match self.body {
            StaticAssignment { expr, .. } | VariableAssignment { expr, .. } => match self.index {
                0 => Some(expr),
                _ => None,
            },
            Return { expr, .. } => match self.index {
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

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum ArithmeticOP {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    GEq,
    LEq,
}

impl fmt::Display for ArithmeticOP {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use ArithmeticOP::*;
        match self {
            Add => write!(f, "+"),
            Sub => write!(f, "-"),
            Mul => write!(f, "*"),
            Div => write!(f, "/"),
            Eq => write!(f, "=="),
            GEq => write!(f, ">="),
            LEq => write!(f, "<="),
        }
    }
}
