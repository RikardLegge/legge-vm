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
    fn node_type(node_id: NodeID<Self>, ast: &Ast) -> Result<NodeType> {
        Err(Error::TypeNotInferred)
    }

    fn children(&self) -> Box<dyn Iterator<Item = &NodeID> + '_> {
        Box::new([].iter())
    }

    fn link(node_id: NodeID<Self>, ast: &mut Ast) -> Result<()> {
        Ok(())
    }
}

pub trait TryFromRef<T: ?Sized> {
    type Error: Debug;

    fn try_from_ref(node: &T) -> Result<&Self, Self::Error>;
}

pub trait TryFromMut<T: ?Sized> {
    type Error: Debug;

    fn try_from_mut(node: &mut T) -> Result<&mut Self, Self::Error>;
}

macro_rules! impl_try_from_ast_node {
    ( $($for: ident),* ) => {
        $(
            impl TryFromRef<AstNodeBody> for $for {
                type Error = ();

                fn try_from_ref(node: &AstNodeBody) -> Result<&Self, Self::Error> {
                    match node {
                        AstNodeBody::$for(ref value) => Ok(value),
                        _ => Err(()),
                    }
                }
            }

            impl TryFromMut<AstNodeBody> for $for {
                type Error = ();

                fn try_from_mut(node: &mut AstNodeBody) -> Result<&mut Self, Self::Error> {
                    match node {
                        AstNodeBody::$for(ref mut value) => Ok(value),
                        _ => Err(()),
                    }
                }
            }

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
                    AstNodeBody::Unknown(Unknown) => unreachable!(),
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
                    AstNodeBody::Unknown(Unknown) => unreachable!(),
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
                    AstNodeBody::Unknown(Unknown) => unreachable!(),
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
