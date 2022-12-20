use std::fmt::Debug;
use std::marker::PhantomData;

// All nodes have to implement this
pub trait NodeBody {
    // The root node to enable simple type erasure.
    type Root: NodeBody + NodeDataStorage;
    // The data type used to store the body of this node. Must cast
    // to the a variant in the Storage enum.
    type Data: Into<<Self::Root as NodeDataStorage>::Storage>;
}

pub trait NodeDataStorage {
    type Storage: Default;
}

/// Implemented by all data types, ensures a 1-1 relation between
/// node and data and prevents the body to be extracted for variants
/// which do not include data.
///
/// # Safety
/// TODO: Does this have to be unsafe?
pub unsafe trait NodeData {
    type Node: NodeBody<Data = Self>;
}

// A typed array index into the Ast
#[repr(transparent)]
#[derive(Debug)]
pub struct NodeID<T: NodeBody + ?Sized>(usize, PhantomData<T>);

impl<T: NodeBody + ?Sized> Into<usize> for NodeID<T> {
    fn into(self) -> usize {
        self.0
    }
}

impl<T: NodeBody> PartialEq<Self> for NodeID<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<T: NodeBody> Eq for NodeID<T> {}

impl<T: NodeBody> Clone for NodeID<T> {
    fn clone(&self) -> Self {
        Self(self.0, Default::default())
    }
}

impl<T> Copy for NodeID<T> where T: NodeBody {}

impl<T: NodeBody> NodeID<T> {
    pub fn new(id: usize) -> Self {
        Self(id, Default::default())
    }
}

pub struct Ast<Any: NodeBody<Root = Any> + NodeDataStorage> {
    nodes: Vec<AstNode<Any>>,
}

impl<Any: NodeBody<Root = Any> + NodeDataStorage> Ast<Any> {
    pub fn new() -> Self {
        Self { nodes: Vec::new() }
    }

    pub fn node<T: NodeData>(
        &mut self,
        parent_id: Option<NodeID<<Any as NodeBody>::Root>>,
        body: impl FnOnce(&mut Self, NodeID<T::Node>) -> T,
    ) -> NodeID<T::Node>
    where
        T: Into<<Any as NodeDataStorage>::Storage>,
        NodeID<T::Node>: Into<NodeID<Any>>,
    {
        let index = self.nodes.len();
        let id = NodeID::new(index);

        // Create placeholder, trying to extract
        // the data inside the `body` function will
        // cause a panic.
        self.nodes.push(AstNode {
            id: id.into(),
            parent_id,
            data: Default::default(),
        });

        let data = body(self, id).into();
        self.nodes[index].data = data;
        id
    }

    pub fn get<T: NodeBody>(&self, id: NodeID<T>) -> &AstNode<T>
    where
        NodeID<T>: Into<NodeID<Any>>,
    {
        let id: NodeID<Any> = id.into();
        let node: &AstNode<Any> = &self.nodes[id.0];
        // Safety: Only affects the marker types on AstNode. The marker
        // traits are never trusted and real conversion checks are always
        // executed when extracting data.
        let node: &AstNode<T> = unsafe { std::mem::transmute(node) };
        node
    }

    pub fn get_mut<T: NodeBody>(&mut self, id: NodeID<T>) -> &mut AstNode<T>
    where
        NodeID<T>: Into<NodeID<Any>>,
    {
        let id: NodeID<Any> = id.into();
        let node: &mut AstNode<Any> = &mut self.nodes[id.0];
        // Safety: Only affects the marker types on AstNode. The marker
        // traits are never trusted and real conversion checks are always
        // executed when extracting data.
        let node: &mut AstNode<T> = unsafe { std::mem::transmute(node) };
        node
    }

    pub fn body<'a, T: NodeBody + 'a>(&'a self, id: NodeID<T>) -> &'a T::Data
    where
        NodeID<T>: Into<NodeID<Any>>,
        T::Data: NodeData,
        &'a <T::Root as NodeDataStorage>::Storage: TryInto<&'a T::Data>,
        <&'a <T::Root as NodeDataStorage>::Storage as TryInto<&'a T::Data>>::Error: Debug,
    {
        self.get(id).body()
    }

    pub fn body_mut<'a, T: NodeBody + 'a>(&'a mut self, id: NodeID<T>) -> &'a mut T::Data
    where
        NodeID<T>: Into<NodeID<Any>>,
        T::Data: NodeData,
        &'a mut <T::Root as NodeDataStorage>::Storage: TryInto<&'a mut T::Data>,
        <&'a mut <T::Root as NodeDataStorage>::Storage as TryInto<&'a mut T::Data>>::Error: Debug,
    {
        self.get_mut(id).body_mut()
    }

    pub fn walk_up<T: NodeBody>(
        &self,
        node_id: impl Into<NodeID<Any>>,
        test: impl Fn(&AstNode<Any>) -> Result<Option<NodeID<T>>, ()>,
    ) -> Result<Option<NodeID<T>>, ()> {
        let mut node_id: NodeID<Any> = node_id.into();
        loop {
            let node = self.get(node_id);
            let result = test(node)?;

            if let Some(node_id) = result {
                break Ok(Some(node_id));
            } else if let Some(parent_id) = node.parent_id {
                node_id = parent_id;
            } else {
                break Ok(None);
            }
        }
    }
}

pub struct AstNodeRef<Body: NodeBody + ?Sized> {
    pub id: NodeID<Body>,
    pub parent_id: Option<NodeID<Body::Root>>,
}

impl<Body: NodeBody> Clone for AstNodeRef<Body> {
    fn clone(&self) -> Self {
        Self {
            id: self.id,
            parent_id: self.parent_id,
        }
    }
}

impl<Body: NodeBody> Copy for AstNodeRef<Body> {}

#[repr(C)]
pub struct AstNode<Body: NodeBody> {
    pub id: NodeID<Body>,
    pub parent_id: Option<NodeID<Body::Root>>,
    data: <Body::Root as NodeDataStorage>::Storage,
}

impl<Body: NodeBody> AstNode<Body> {
    pub fn storage(&self) -> &<Body::Root as NodeDataStorage>::Storage {
        &self.data
    }

    pub fn get_ref(&self) -> AstNodeRef<Body> {
        AstNodeRef {
            id: self.id,
            parent_id: self.parent_id,
        }
    }

    pub fn body<'a>(&'a self) -> &'a Body::Data
    where
        Body::Data: NodeData,
        &'a <Body::Root as NodeDataStorage>::Storage: TryInto<&'a Body::Data>,
        <&'a <Body::Root as NodeDataStorage>::Storage as TryInto<&'a Body::Data>>::Error: Debug,
    {
        let storage: &<Body::Root as NodeDataStorage>::Storage = &self.data;
        let body: &Body::Data = storage.try_into().unwrap();
        body
    }

    pub fn body_mut<'a>(&'a mut self) -> &'a mut Body::Data
    where
        Body::Data: NodeData,
        &'a mut <Body::Root as NodeDataStorage>::Storage: TryInto<&'a mut Body::Data>,
        <&'a mut <Body::Root as NodeDataStorage>::Storage as TryInto<&'a mut Body::Data>>::Error:
            Debug,
    {
        let storage: &mut <Body::Root as NodeDataStorage>::Storage = &mut self.data;
        let body: &mut Body::Data = storage.try_into().unwrap();
        body
    }
}
