use crate::ast::{AnyNode, Ast, NodeIterator};
use crate::Error;
use once_cell::unsync::OnceCell;
use std::fmt::{Debug, Formatter};
use std::marker::PhantomData;

impl<Body> AstNode<AnyNode, Body>
where
    Body: NodeBody,
    NodeID<Body>: From<NodeID>,
{
    pub fn new(id: impl Into<NodeID>, parent_id: Option<impl Into<NodeID>>) -> Self {
        Self {
            id: id.into(),
            parent_id: parent_id.map(|id| id.into()),
            node_type: NodeTypeUsages {
                tp: OnceCell::new(),
                call: OnceCell::new(),
                value: OnceCell::new(),
            },
            body: None,
        }
    }

    pub fn check(node_id: NodeID, ast: &mut Ast<Body::Root>) -> crate::Result<()> {
        Body::check(node_id.into(), ast)
    }

    pub fn link(
        node_id: NodeID,
        ast: &mut Ast<Body::Root>,
        context: Body::AstContext,
    ) -> crate::Result<()> {
        Body::link(node_id.into(), ast, context)
    }

    pub fn node_type(
        node_id: NodeID,
        ast: &Ast<Body::Root>,
        usage: NodeUsage,
    ) -> crate::Result<Body::NodeType> {
        Body::node_type(node_id.into(), ast, usage)
    }
}

#[derive(Debug)]
struct NodeTypeUsages<NodeType> {
    pub tp: OnceCell<NodeType>,
    pub call: OnceCell<NodeType>,
    pub value: OnceCell<NodeType>,
}

#[derive(Debug)]
#[repr(C)]
pub struct AstNode<InnerType, Body>
where
    Body: NodeBody,
{
    pub id: NodeID<InnerType>,
    pub parent_id: Option<NodeID>,
    pub node_type: NodeTypeUsages<Body::NodeType>,
    pub body: Option<Body>,
}

impl<InnerType, Body> AstNode<InnerType, Body>
where
    Body: NodeBody,
{
    pub fn children(&self, context: Body::AstContext) -> NodeIterator<'_, Body::AstContext> {
        match self.body.as_ref() {
            None => NodeIterator::empty(),
            Some(body) => body.children(context),
        }
    }
}

pub trait NodeBody: Sized + Debug + Clone {
    type Root: NodeBody;
    type NodeType: Debug;
    type AstContext: Default + Debug + Copy;
    type Variable: NodeBody;

    fn node_type(
        node_id: NodeID<Self>,
        _ast: &Ast<Self::Root>,
        _usage: NodeUsage,
    ) -> crate::Result<Self::NodeType> {
        Err(Error::TypeNotInferred(node_id.into()))
    }

    fn children(&self, _context: Self::AstContext) -> NodeIterator<'_, Self::AstContext> {
        NodeIterator::empty()
    }

    fn link(
        _node_id: NodeID<Self>,
        _ast: &mut Ast<Self::Root>,
        _context: Self::AstContext,
    ) -> crate::Result<()> {
        Ok(())
    }

    fn check(_node_id: NodeID<Self>, _ast: &mut Ast<Self::Root>) -> crate::Result<()> {
        Ok(())
    }

    fn has_variable(&self, _var: &str) -> crate::Result<Option<NodeID<Self::Variable>>> {
        Ok(None)
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum NodeUsage {
    Type,
    Call,
    Value,
}

/// NodeID is repr(transparent) to ensure that it's safe to cast between
/// different marker types.
#[repr(transparent)]
pub struct NodeID<T = AnyNode> {
    id: usize,
    _tp: PhantomData<fn() -> T>,
}

#[derive(Debug)]
#[repr(C)]
pub struct NodeIDContext<AstContext: Copy + Debug> {
    pub node_id: NodeID,
    pub context: AstContext,
}

impl<AstContext: Copy + Debug> NodeIDContext<AstContext> {
    pub fn new(node_id: impl Into<NodeID>, context: AstContext) -> Self {
        Self {
            node_id: node_id.into(),
            context,
        }
    }
}

impl<AstContext: Copy + Debug> Clone for NodeIDContext<AstContext> {
    fn clone(&self) -> Self {
        Self {
            node_id: self.node_id,
            context: self.context,
        }
    }
}

impl<T: Copy + Debug> Copy for NodeIDContext<T> {}

impl<Source, AstContext> From<NodeID<Source>> for NodeIDContext<AstContext>
where
    Source: NodeBody,
    AstContext: Copy + Debug + Default,
{
    fn from(node: NodeID<Source>) -> Self {
        Self {
            node_id: node.into(),
            context: AstContext::default(),
        }
    }
}

impl<AstContext> From<NodeID> for NodeIDContext<AstContext>
where
    AstContext: Copy + Debug + Default,
{
    fn from(node: NodeID) -> Self {
        Self {
            node_id: node.into(),
            context: AstContext::default(),
        }
    }
}

impl<T> PartialEq for NodeID<T> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl<T> Eq for NodeID<T> {}

impl<T> Debug for NodeID<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Node({})", self.id)
    }
}

impl<T> NodeID<T> {
    pub fn id(&self) -> usize {
        self.id
    }
}

impl<T> From<NodeID<T>> for NodeID
where
    T: NodeBody,
{
    fn from(id: NodeID<T>) -> Self {
        // Erase the type of the NodeID, this is practical when used as for example keys
        // in a hashmap or when stored in an mixed type array.
        //
        // Safety: T is only a marker trait and does not affect the shape of the struct.
        // NodeID is also repr(transparent) to ensure that the marker trait does not
        // have an affect on the layout in future versions.
        unsafe { std::mem::transmute(id) }
    }
}

impl<T> Clone for NodeID<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for NodeID<T> {}

impl<T> NodeID<T> {
    pub fn new(id: usize) -> NodeID<T> {
        NodeID {
            id,
            _tp: Default::default(),
        }
    }
}
