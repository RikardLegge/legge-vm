mod block;
mod expression;
mod node_id;
mod statement;
mod variable;

pub use block::*;
pub use expression::*;
pub use node_id::*;
pub use statement::*;
pub use variable::*;

use crate::{Ast, Error, Result};
use std::fmt::Debug;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum NodeType {
    Void,
    Int,
    Float,
    String,
    Custom(NodeID<TypeDeclaration>),
    Function(NodeID<FunctionDeclaration>),
}

enum NodeIteratorBody<'a> {
    Empty,
    Single(NodeID),
    Dual(NodeID, NodeID),
    Slice(&'a [NodeID]),
    Chained(Box<NodeIterator<'a>>, Box<NodeIterator<'a>>),
}

pub struct NodeIterator<'a> {
    index: usize,
    items: NodeIteratorBody<'a>,
}

impl<'a> NodeIterator<'a> {
    fn new(items: NodeIteratorBody<'a>) -> NodeIterator<'a> {
        Self { index: 0, items }
    }

    pub fn empty() -> NodeIterator<'a> {
        Self::new(NodeIteratorBody::Empty)
    }

    pub fn single(first: impl Into<NodeID>) -> NodeIterator<'a> {
        Self::new(NodeIteratorBody::Single(first.into()))
    }

    pub fn dual(first: impl Into<NodeID>, second: impl Into<NodeID>) -> NodeIterator<'a> {
        Self::new(NodeIteratorBody::Dual(first.into(), second.into()))
    }

    pub fn chained(first: NodeIterator<'a>, second: NodeIterator<'a>) -> NodeIterator<'a> {
        Self::new(NodeIteratorBody::Chained(Box::new(first), Box::new(second)))
    }

    pub fn slice<T>(slice: &[NodeID<T>]) -> NodeIterator<'a> {
        // Safety: A slice is a contiguous piece of memory and can not change it's
        // bit representation. NodeID is also repr(transparent) to ensure that the
        // marker trait does not have an affect on the layout in future versions.
        let type_erased_slice: &[NodeID] = unsafe { std::mem::transmute(slice) };
        Self::new(NodeIteratorBody::Slice(type_erased_slice))
    }
}

impl<'a> Iterator for NodeIterator<'a> {
    type Item = NodeID;

    fn next(&mut self) -> Option<Self::Item> {
        let result = match self.items {
            NodeIteratorBody::Single(item) if self.index == 0 => Some(item),

            NodeIteratorBody::Dual(item, _) if self.index == 0 => Some(item),
            NodeIteratorBody::Dual(_, item) if self.index == 1 => Some(item),

            NodeIteratorBody::Slice(slice) => slice.get(self.index).cloned(),

            NodeIteratorBody::Chained(ref mut first, ref mut second) => first.chain(second).next(),

            _ => None,
        };
        if result.is_some() {
            self.index += 1;
        }
        result
    }
}

pub trait Node: Sized {
    fn node_type(_: NodeID<Self>, _: &Ast) -> Result<NodeType> {
        Err(Error::TypeNotInferred)
    }

    fn children(&self) -> NodeIterator<'_> {
        NodeIterator::empty()
    }

    fn link(_: NodeID<Self>, _: &mut Ast) -> Result<()> {
        Ok(())
    }
}

#[macro_export]
macro_rules! try_cast_node {
    ($node:ident as $ty:tt) => {
        match $node.body.as_ref().unwrap() {
            $crate::node::AstNodeBody::$ty(_) => {
                // Safety: The node must have a body of type $ty, since AstNode
                // is repr(C), it must adhere to the C layout ABI and therefore
                // the marker trait <T> will not change the binary representation.
                let block: &$crate::node::AstNode<$ty> = unsafe { std::mem::transmute($node) };
                Some(block)
            }
            _ => None,
        }
    };
}

#[macro_export]
macro_rules! impl_try_from_variant {
    ( impl $from:ident for $to:ident throws $error:ident :: $error_variant:ident ) => {
        impl<'a> TryFrom<&'a $from> for &'a $to {
            type Error = $error;

            fn try_from(node: &'a $from) -> Result<Self, Self::Error> {
                match node {
                    $from::$to(ref value) => Ok(value),
                    _ => Err($error::$error_variant),
                }
            }
        }

        impl<'a> TryFrom<&'a mut $from> for &'a mut $to {
            type Error = $error;

            fn try_from(node: &'a mut $from) -> Result<Self, Self::Error> {
                match node {
                    $from::$to(ref mut value) => Ok(value),
                    _ => Err($error::$error_variant),
                }
            }
        }

        impl TryFrom<$from> for $to {
            type Error = $error;

            fn try_from(node: $from) -> Result<Self, Self::Error> {
                match node {
                    $from::$to(value) => Ok(value),
                    _ => Err($error::$error_variant),
                }
            }
        }
    };
}

#[macro_export]
macro_rules! impl_enum_node {
    ( pub enum $enum:ident { $($variant:ident),* $(,)* } ) => {
        #[derive(Debug)]
        pub enum $enum {
             $($variant($variant)),*
        }

        $(
            $crate::impl_try_from_variant!(impl $enum for $variant throws Error::InternalError);

            impl From<$variant> for $enum {
                fn from(op: $variant) -> Self {
                    $enum::$variant(op)
                }
            }

            impl From<NodeID<$variant>> for NodeID<$enum> {
                fn from(node_id: NodeID<$variant>) -> Self {
                    unsafe {std::mem::transmute(node_id)}
                }
            }

            impl From<$variant> for crate::node::AstNodeBody {
                fn from(variant: $variant) -> Self {
                    crate::node::AstNodeBody::$enum($enum::$variant(variant))
                }
            }

            impl<'a> TryFrom<&'a $crate::node::AstNodeBody> for &'a $variant {
                type Error = $crate::Error;

                fn try_from(body: &'a $crate::node::AstNodeBody) -> std::result::Result<Self, Self::Error> {
                    let value = <&$enum>::try_from(body).map_err(|_| $crate::Error::InternalError)?;
                    match value {
                        $enum::$variant(ref value) => Ok(value),
                        _ => Err(Error::InternalError)
                    }
                }
            }

            impl<'a> TryFrom<&'a mut $crate::node::AstNodeBody> for &'a mut $variant {
                type Error = $crate::Error;

                fn try_from(body: &'a mut $crate::node::AstNodeBody) -> std::result::Result<Self, Self::Error> {
                    let value = <&mut $enum>::try_from(body).map_err(|_| $crate::Error::InternalError)?;
                    match value {
                        $enum::$variant(ref mut value) => Ok(value),
                        _ => Err(Error::InternalError)
                    }
                }
            }

            impl TryFrom<$crate::node::AstNodeBody> for $variant {
                type Error = $crate::Error;

                fn try_from(body: $crate::node::AstNodeBody) -> std::result::Result<Self, Self::Error> {
                    let value = <$enum>::try_from(body).map_err(|_| $crate::Error::InternalError)?;
                    match value {
                        $enum::$variant(value) => Ok(value),
                        _ => Err(Error::InternalError)
                    }
                }
            }

        )*

        impl Node for $enum {
            fn node_type(node_id: NodeID<Self>, ast: &Ast) -> Result<NodeType> {
                match ast.get_inner(node_id) {
                    $(
                        $enum::$variant(_) => {
                            let node_id: NodeID<$variant> = unsafe {std::mem::transmute(node_id) };
                            $variant::node_type(node_id, ast)
                        }
                    ),*
                }
            }

            fn children(&self) -> NodeIterator<'_> {
                match &self {
                    $(
                        $enum::$variant(value) => value.children()
                    ),*
                }
            }

            fn link(node_id: NodeID<Self>, ast: &mut Ast) -> Result<()> {
                match ast.get_inner(node_id) {
                    $(
                        $enum::$variant(_) => {
                            let node_id: NodeID<$variant> = unsafe {std::mem::transmute(node_id) };
                            $variant::link(node_id, ast)
                        }
                    ),*
                }
            }
        }
    }
}

macro_rules! impl_try_from_ast_node {
    ( $($variant: ident),* ) => {
        $(
            impl_try_from_variant!(impl AstNodeBody for $variant throws Error::InternalError);

            impl From<$variant> for AstNodeBody {
                fn from(node: $variant) -> Self {
                    AstNodeBody::$variant(node)
                }
            }

            impl AstNode<$variant> {
                pub fn body(&self) -> &$variant {
                    match self.body.as_ref().unwrap() {
                        AstNodeBody::$variant(inner) => inner,
                        _ => unreachable!(),
                    }
                }
            }
        )*

        #[derive(Debug)]
        pub enum AstNodeBody {
            Unknown(Unknown),
            $($variant($variant)),*
        }

        impl AstNode {
            pub fn link(node_id: NodeID, ast: &mut Ast) -> Result<()> {
                let node = ast.get(node_id);
                match node.body.as_ref().unwrap() {
                    AstNodeBody::Unknown(_) => unreachable!(),
                    $(
                        AstNodeBody::$variant(_) => {
                            let node = crate::try_cast_node!(node as $variant).unwrap();
                            $variant::link(node.id, ast)
                        }
                    ),*

                }
            }

            pub fn node_type(node_id: NodeID, ast: &Ast) -> Result<NodeType> {
                let node = ast.get(node_id);
                match node.body.as_ref().unwrap() {
                    AstNodeBody::Unknown(_) => unreachable!(),
                    $(
                        AstNodeBody::$variant(_) => {
                            let node = crate::try_cast_node!(node as $variant).unwrap();
                            $variant::node_type(node.id, ast)
                        }
                    ),*

                }
            }
        }

        impl AstNodeBody {
            pub fn children(&self) -> NodeIterator<'_> {
                match self {
                    AstNodeBody::Unknown(_) => unreachable!(),
                    $(
                        AstNodeBody::$variant(ref inner) => inner.children()
                    ),*
                }
            }
        }
    }
}

pub type Unknown = ();

#[derive(Debug)]
#[repr(C)]
pub struct AstNode<NodeType = Unknown> {
    pub id: NodeID<NodeType>,
    pub parent_id: Option<NodeID>,
    pub body: Option<AstNodeBody>,
}

impl<T> AstNode<T> {
    pub fn children(&self) -> NodeIterator<'_> {
        self.body.as_ref().unwrap().children()
    }

    pub fn set_parent(&mut self, parent_id: impl Into<NodeID>) {
        self.parent_id = Some(parent_id.into());
    }
}

impl_try_from_ast_node![Block, Statement, Expression, Variable];
