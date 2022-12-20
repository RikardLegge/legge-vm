use crate::ast::{AstNode, AstNodeRef, NodeBody};
use crate::linker::{LinkContext, Linker};
use crate::node::{Ast, NodeID};

// Example of dynamically adding methods to the AST, which respect the node types
pub trait Children {
    fn children<'this, 'ast>(&'this self, ast: &'ast Ast) -> ChildIterator<'ast>;
}

impl<Any: NodeBody> Children for AstNode<Any>
where
    AstNodeRef<Any>: Children,
{
    fn children<'this, 'ast>(&'this self, ast: &'ast Ast) -> ChildIterator<'ast> {
        self.get_ref().children(ast)
    }
}

pub struct ChildIteratorElementSlice<'children>(&'children [NodeID]);

impl<'children> ChildIteratorElementSlice<'children> {
    fn get(&self, index: usize) -> Option<NodeID> {
        self.0.get(index).cloned()
    }
}

impl<'children, Any: NodeBody> From<&'children [NodeID<Any>]> for ChildIteratorItem<'children>
where
    NodeID<Any>: Into<NodeID>,
{
    fn from(slice: &'children [NodeID<Any>]) -> Self {
        // Safety: A slice is a contiguous piece of memory and can not change it's
        // bit representation. NodeID is also repr(transparent) to ensure that the
        // marker trait does not have an affect on the layout in future versions.
        let type_erased_slice: &[NodeID] = unsafe { std::mem::transmute(slice) };
        let item = ChildIteratorElementSlice(type_erased_slice);
        ChildIteratorItem::Slice(item)
    }
}

pub struct ChildIteratorElementValue(NodeID);

impl ChildIteratorElementValue {
    fn get(&self, index: usize) -> Option<NodeID> {
        if index == 0 {
            Some(self.0)
        } else {
            None
        }
    }
}

impl<'children, Any: NodeBody> From<NodeID<Any>> for ChildIteratorItem<'children>
where
    NodeID<Any>: Into<NodeID>,
{
    fn from(id: NodeID<Any>) -> Self {
        let item = ChildIteratorElementValue(id.into());
        ChildIteratorItem::Value(item)
    }
}

pub enum ChildIteratorItem<'children> {
    Final,
    Slice(ChildIteratorElementSlice<'children>),
    Value(ChildIteratorElementValue),
}

pub struct ChildIteratorItems<'children>([ChildIteratorItem<'children>; 4]);

impl<'children> From<[ChildIteratorItem<'children>; 0]> for ChildIteratorItems<'children> {
    fn from(_: [ChildIteratorItem<'children>; 0]) -> Self {
        [ChildIteratorItem::Final].into()
    }
}
impl<'children> From<[ChildIteratorItem<'children>; 1]> for ChildIteratorItems<'children> {
    fn from([a]: [ChildIteratorItem<'children>; 1]) -> Self {
        [a, ChildIteratorItem::Final].into()
    }
}
impl<'children> From<[ChildIteratorItem<'children>; 2]> for ChildIteratorItems<'children> {
    fn from([a, b]: [ChildIteratorItem<'children>; 2]) -> Self {
        [a, b, ChildIteratorItem::Final].into()
    }
}
impl<'children> From<[ChildIteratorItem<'children>; 3]> for ChildIteratorItems<'children> {
    fn from([a, b, c]: [ChildIteratorItem<'children>; 3]) -> Self {
        [a, b, c, ChildIteratorItem::Final].into()
    }
}
impl<'children> From<[ChildIteratorItem<'children>; 4]> for ChildIteratorItems<'children> {
    fn from([a, b, c, d]: [ChildIteratorItem<'children>; 4]) -> Self {
        ChildIteratorItems([a, b, c, d])
    }
}

pub struct ChildIterator<'children> {
    item: usize,
    index: usize,
    items: ChildIteratorItems<'children>,
}

impl<'ast> ChildIterator<'ast> {
    pub fn new(items: ChildIteratorItems<'ast>) -> ChildIterator<'ast> {
        Self {
            item: 0,
            index: 0,
            items,
        }
    }
}

impl<'a> Iterator for ChildIterator<'a> {
    type Item = NodeID;

    fn next(&mut self) -> Option<Self::Item> {
        for item in self.items.0.iter().skip(self.item) {
            let res = match item {
                ChildIteratorItem::Final => return None,
                ChildIteratorItem::Slice(slice) => slice.get(self.index),
                ChildIteratorItem::Value(value) => value.get(self.index),
            };

            match res {
                Some(res) => {
                    self.index += 1;
                    return Some(res);
                }
                None => {
                    self.index = 0;
                    self.item += 1;
                }
            }
        }
        None
    }
}
