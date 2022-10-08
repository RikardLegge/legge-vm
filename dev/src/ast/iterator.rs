use crate::ast::{NodeID, NodeIDContext};
use std::fmt::Debug;

pub enum NodeIteratorBody<'a, AstContext: Copy + Debug> {
    Empty,
    Single(NodeIDContext<AstContext>),
    Dual(NodeIDContext<AstContext>, NodeIDContext<AstContext>),
    Slice(&'a [NodeID]),
    Chained(
        Box<NodeIterator<'a, AstContext>>,
        Box<NodeIterator<'a, AstContext>>,
    ),
}

pub struct NodeIterator<'a, AstContext: Copy + Debug> {
    index: usize,
    items: NodeIteratorBody<'a, AstContext>,
}

impl<'a, AstContext: Copy + Debug> NodeIterator<'a, AstContext> {
    pub fn new(items: NodeIteratorBody<'a, AstContext>) -> NodeIterator<'a, AstContext> {
        Self { index: 0, items }
    }

    pub fn empty() -> NodeIterator<'a, AstContext> {
        Self::new(NodeIteratorBody::Empty)
    }

    pub fn single(first: impl Into<NodeIDContext<AstContext>>) -> NodeIterator<'a, AstContext> {
        Self::new(NodeIteratorBody::Single(first.into()))
    }

    pub fn dual(
        first: impl Into<NodeIDContext<AstContext>>,
        second: impl Into<NodeIDContext<AstContext>>,
    ) -> NodeIterator<'a, AstContext> {
        Self::new(NodeIteratorBody::Dual(first.into(), second.into()))
    }

    pub fn chained(
        first: NodeIterator<'a, AstContext>,
        second: NodeIterator<'a, AstContext>,
    ) -> NodeIterator<'a, AstContext> {
        Self::new(NodeIteratorBody::Chained(Box::new(first), Box::new(second)))
    }

    pub fn slice<T>(slice: &[NodeID<T>]) -> NodeIterator<'a, AstContext> {
        // Safety: A slice is a contiguous piece of memory and can not change it's
        // bit representation. NodeID is also repr(transparent) to ensure that the
        // marker trait does not have an affect on the layout in future versions.
        let type_erased_slice: &[NodeID] = unsafe { std::mem::transmute(slice) };
        Self::new(NodeIteratorBody::Slice(type_erased_slice))
    }
}

impl<'a, AstContext: Copy + Debug + Default> Iterator for NodeIterator<'a, AstContext> {
    type Item = NodeIDContext<AstContext>;

    fn next(&mut self) -> Option<Self::Item> {
        let result = match self.items {
            NodeIteratorBody::Single(item) if self.index == 0 => Some(item),

            NodeIteratorBody::Dual(item, _) if self.index == 0 => Some(item),
            NodeIteratorBody::Dual(_, item) if self.index == 1 => Some(item),

            NodeIteratorBody::Slice(slice) => {
                slice.get(self.index).cloned().map(|node| NodeIDContext {
                    node_id: node,
                    context: AstContext::default(),
                })
            }

            NodeIteratorBody::Chained(ref mut first, ref mut second) => first.chain(second).next(),
            _ => None,
        };
        if result.is_some() {
            self.index += 1;
        }
        result
    }
}
