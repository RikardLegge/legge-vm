use crate::ast::ast::{PartialNodeValue, PartialType, StateAny};
use crate::ast::NodeID;
use crate::token::ArithmeticOP;

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
pub enum NodeBody<T = StateAny> {
    Empty,
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
        body: Vec<NodeID>,
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
        ident: String,
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
    TypeDeclaration {
        ident: String,
        tp: NodeID,
        constructor: NodeID,
        default_value: Option<PartialNodeValue<T>>,
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

    Unlinked(UnlinkedNodeBody<T>),
}

impl<T> NodeBody<T> {
    pub fn children(&self) -> NodeBodyIterator<T> {
        NodeBodyIterator {
            index: 0,
            body: self,
            unlinked: None,
        }
    }
}

#[derive(Debug, Clone)]
pub enum UnlinkedNodeBody<T = StateAny> {
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
        args: Vec<NodeID>,
    },
    ImportValue {
        ident: String,
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

pub struct NodeBodyIterator<'a, T = StateAny> {
    index: usize,
    body: &'a NodeBody<T>,
    unlinked: Option<UnlinkedNodeBodyIterator<'a, T>>,
}

impl<'a, T> Iterator for NodeBodyIterator<'a, T> {
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
            Block { body } => body.get(self.index),
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
            | Import { expr: value, .. } => match self.index {
                0 => Some(value),
                _ => None,
            },
            TypeDeclaration {
                constructor, tp, ..
            } => match self.index {
                0 => Some(constructor),
                1 => Some(tp),
                _ => None,
            },
            VariableDeclaration { expr: None, tp, .. } => match self.index {
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
            TypeReference { .. } | VariableValue { .. } | Comment { .. } | Break { .. } | Empty => {
                None
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
        };
        if option.is_some() {
            self.index += 1;
        }
        option
    }
}

pub struct UnlinkedNodeBodyIterator<'a, T = StateAny> {
    index: usize,
    body: &'a UnlinkedNodeBody<T>,
}

impl<'a, T> Iterator for UnlinkedNodeBodyIterator<'a, T> {
    type Item = &'a NodeID;

    fn next(&mut self) -> Option<&'a NodeID> {
        use UnlinkedNodeBody::*;
        let option = match self.body {
            VariableAssignment { expr, .. } => match self.index {
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
