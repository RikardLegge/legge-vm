use std::fmt::Debug;
use std::marker::PhantomData;

#[repr(transparent)]
#[derive(Debug)]
pub struct NodeID<T: NodeBody + ?Sized>(usize, PhantomData<T>);

pub trait NodeData {
    type Node: NodeBody<Data = Self>;
}

pub trait NodeBody: Debug {
    // The root node to enable simple type erasure.
    type Root: NodeBody;
    // The data type used to store the body of this node. Must cast
    // to the a variant in the Storage enum.
    type Data: Debug;
}

#[derive(Debug)]
struct Root {}
impl NodeBody for Root {
    type Root = Root;
    type Data = ();
}

struct Builder<Body: NodeBody>
where
    NodeID<Body>: Into<NodeID<Root>>,
{
    id: PhantomData<Body>,
    ast: Ast2<Root>,
}

impl<Body: NodeBody> Builder<Body>
where
    NodeID<Body>: Into<NodeID<Root>>,
{
    fn typed_variable(&mut self) {
        let _ = self.ast.node_body::<VariableStorage2>(VariableStorage2 {});
        let _ = self.ast.node_body(VariableStorage2 {});
    }
}

#[derive(Debug)]
pub struct Ast2<Any: NodeBody<Root = Any>> {
    nodes: PhantomData<Any>,
}

impl<Any: NodeBody<Root = Any>> Ast2<Any> {
    pub fn node_body<T: NodeData>(&mut self, _: T) -> NodeID<T::Node>
    where
        NodeID<T::Node>: Into<NodeID<Any>>,
    {
        unimplemented!()
    }
}

#[derive(Debug)]
pub struct VariableStorage2 {}

impl From<NodeID<Variable2>> for NodeID<Root> {
    fn from(_: NodeID<Variable2>) -> Self {
        todo!()
    }
}

#[derive(Debug)]
pub struct Variable2 {}

impl NodeBody for Variable2 {
    type Root = Root;
    type Data = VariableStorage2;
}

impl NodeData for VariableStorage2 {
    type Node = Variable2;
}
