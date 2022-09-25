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

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum NodeType {
    Void,
    Int,
    Float,
    String,
}

pub trait Node: Into<AstNodeBody> {
    fn node_type(_: NodeID<Self>, _: &Ast) -> Result<NodeType> {
        Err(Error::TypeNotInferred)
    }

    fn children(&self) -> Box<dyn Iterator<Item = &NodeID> + '_> {
        Box::new([].iter())
    }

    fn link(_: NodeID<Self>, _: &mut Ast) -> Result<()> {
        Ok(())
    }
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

        )*

        impl Node for $enum {
            fn node_type(node_id: NodeID<Self>, ast: &Ast) -> Result<NodeType> {
                match ast.get_inner(node_id) {
                    $(
                        $enum::$variant(_) => $variant::node_type(node_id, ast)
                    ),*
                }
            }

            fn children(&self) -> Box<dyn Iterator<Item = &NodeID> + '_> {
                match &self {
                    $(
                        $enum::$variant(value) => value.children()
                    ),*
                }
            }

            fn link(id: NodeID<Self>, ast: &mut Ast) -> Result<()> {
                match ast.get_inner(id) {
                    $(
                        $enum::$variant(_) => $variant::link(id, ast)
                    ),*
                }
            }
        }
    }
}

macro_rules! impl_try_from_ast_node {
    ( $($for: ident),* ) => {
        $(
            impl_try_from_variant!(impl AstNodeBody for $for throws Error::InternalError);

            impl From<$for> for AstNodeBody {
                fn from(node: $for) -> Self {
                    AstNodeBody::$for(node)
                }
            }

            impl AstNode<$for> {
                pub fn body(&self) -> &$for {
                    match self.body.as_ref().unwrap() {
                        AstNodeBody::$for(inner) => inner,
                        _ => unreachable!(),
                    }
                }
            }
        )*

        #[derive(Debug)]
        pub enum AstNodeBody {
            Unknown(Unknown),
            $($for($for)),*
        }

        impl AstNode {
            pub fn link(node_id: NodeID, ast: &mut Ast) -> Result<()> {
                let node = ast.get(node_id);
                match node.body.as_ref().unwrap() {
                    AstNodeBody::Unknown(_) => unreachable!(),
                    $(
                        AstNodeBody::$for(_) => {
                            let node_id: NodeID<$for> = unsafe {std::mem::transmute(node.id) };
                            $for::link(node_id, ast)
                        }
                    ),*

                }
            }

            pub fn node_type(node_id: NodeID, ast: &Ast) -> Result<NodeType> {
                let node = ast.get(node_id);
                match node.body.as_ref().unwrap() {
                    AstNodeBody::Unknown(_) => unreachable!(),
                    $(
                        AstNodeBody::$for(_) => {
                            let node_id: NodeID<$for> = unsafe {std::mem::transmute(node.id) };
                            $for::node_type(node_id, ast)
                        }
                    ),*

                }
            }
        }

        impl AstNodeBody {
            pub fn children(&self) -> Box<dyn Iterator<Item = &NodeID> + '_> {
                match self {
                    AstNodeBody::Unknown(_) => unreachable!(),
                    $(
                        AstNodeBody::$for(ref inner) => inner.children()
                    ),*
                }
            }
        }
    }
}

pub type Unknown = ();

#[derive(Debug)]
pub struct AstNode<T = Unknown> {
    pub id: NodeID<T>,
    pub parent_id: Option<NodeID>,
    pub body: Option<AstNodeBody>,
}

impl<T> AstNode<T> {
    pub fn children(&self) -> Box<dyn Iterator<Item = &NodeID> + '_> {
        self.body.as_ref().unwrap().children()
    }
}

impl_try_from_ast_node![Block, Statement, Expression, Variable];
