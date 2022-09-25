use crate::node::{Node, Unknown};
use std::fmt::{Debug, Formatter};
use std::marker::PhantomData;

pub struct NodeID<T = Unknown> {
    id: usize,
    _tp: PhantomData<fn() -> T>,
}

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

impl<T> From<NodeID<T>> for NodeID<()>
where
    T: Node,
{
    fn from(id: NodeID<T>) -> Self {
        unsafe { std::mem::transmute(id) }
    }
}

impl<T> From<&NodeID<T>> for &NodeID<()>
where
    T: Node,
{
    fn from(id: &NodeID<T>) -> Self {
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
