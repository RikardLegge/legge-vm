use crate::ast::AstContext;
use crate::node::NodeIDContext;
use crate::NodeID;

pub enum NodeIteratorBody<'a> {
    Empty,
    Single(NodeIDContext),
    Dual(NodeIDContext, NodeIDContext),
    Slice(&'a [NodeID]),
    Chained(Box<NodeIterator<'a>>, Box<NodeIterator<'a>>),
}

pub struct NodeIterator<'a> {
    index: usize,
    items: NodeIteratorBody<'a>,
}

impl<'a> NodeIterator<'a> {
    pub fn new(items: NodeIteratorBody<'a>) -> NodeIterator<'a> {
        Self { index: 0, items }
    }

    pub fn empty() -> NodeIterator<'a> {
        Self::new(NodeIteratorBody::Empty)
    }

    pub fn single(first: impl Into<NodeIDContext>) -> NodeIterator<'a> {
        Self::new(NodeIteratorBody::Single(first.into()))
    }

    pub fn dual(
        first: impl Into<NodeIDContext>,
        second: impl Into<NodeIDContext>,
    ) -> NodeIterator<'a> {
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
    type Item = NodeIDContext;

    fn next(&mut self) -> Option<Self::Item> {
        let result = match self.items {
            NodeIteratorBody::Single(item) if self.index == 0 => Some(item),

            NodeIteratorBody::Dual(item, _) if self.index == 0 => Some(item),
            NodeIteratorBody::Dual(_, item) if self.index == 1 => Some(item),

            NodeIteratorBody::Slice(slice) => {
                slice.get(self.index).cloned().map(|node| NodeIDContext {
                    node_id: node,
                    context: AstContext::Default,
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
