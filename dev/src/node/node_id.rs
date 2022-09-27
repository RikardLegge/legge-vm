use crate::node::{Node, Unknown};
use std::fmt::{Debug, Formatter};
use std::marker::PhantomData;

/// NodeID is repr(transparent) to ensure that it's safe to cast between
/// different marker types.
#[repr(transparent)]
pub struct NodeID<T = Unknown> {
    id: usize,
    _tp: PhantomData<fn() -> T>,
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

impl<T> From<NodeID<T>> for NodeID<Unknown>
where
    T: Node,
{
    fn from(id: NodeID<T>) -> Self {
        let id: &Self = (&id).into();
        *id
    }
}

impl<T> From<&NodeID<T>> for &NodeID<Unknown>
where
    T: Node,
{
    fn from(id: &NodeID<T>) -> Self {
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
