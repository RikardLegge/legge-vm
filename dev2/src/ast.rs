use std::fmt::Debug;
use std::marker::PhantomData;

// All nodes have to implement this
pub trait NodeBody {
    // The root node to enable simple type erasure.
    type Root: NodeBody<Variants = Self::Variants> + NodeDataStorage;
    // An enum defining all possible variants in the current AST tree
    type Variants;
    // The data type used to store the body of this node. Must cast
    // to the a variant in the Storage enum.
    type Data: Into<<Self::Root as NodeDataStorage>::Storage>;

    // Each node type must implement this and ensure that it returns
    // the correct variant.
    // TODO: Maybe mark it unsafe?
    fn variant() -> Self::Variants;
}

pub trait NodeDataStorage {
    type Storage: Default;
}

// Implemented by all data types, ensures a 1-1 relation between
// node and data and prevents the body to be extracted for variants
// which do not include data.
pub trait NodeData {
    type Node: NodeBody<Data = Self>;
}

// A typed array index into the Ast
#[repr(transparent)]
pub struct NodeID<T: NodeBody + ?Sized>(usize, PhantomData<T>);

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

pub struct Ast<Root: NodeBody> {
    nodes: Vec<AstNode<Root>>,
}

impl<Root: NodeBody> Ast<Root> {
    pub fn new() -> Self {
        Self { nodes: Vec::new() }
    }

    pub fn node<T>(
        &mut self,
        parent_id: Option<NodeID<<Root as NodeBody>::Root>>,
        body: impl FnOnce(&mut Self) -> T,
    ) -> NodeID<T::Node>
    where
        T: NodeData,
        T::Node: NodeBody,
        T: Into<<Root::Root as NodeDataStorage>::Storage>,
        <T::Node as NodeBody>::Variants: Into<<Root as NodeBody>::Variants>,
    {
        let index = self.nodes.len();
        let id = NodeID::new(index);
        let node = AstNode::<Root> {
            id,
            variant: <T::Node as NodeBody>::variant().into(),
            parent_id,
            data: None,
        };
        self.nodes.push(node);
        let data = body(self).into();
        self.nodes[index].data = Some(data);
        NodeID::new(index)
    }

    pub fn get_ref<T: NodeBody>(&self, id: NodeID<T>) -> &AstNode<T>
    where
        T::Data: Into<<Root::Root as NodeDataStorage>::Storage>,
    {
        let node = &self.nodes[id.0];
        assert!(node.data.is_some());
        let node: &AstNode<T> = unsafe { std::mem::transmute(node) };
        node
    }

    pub fn get_mut<T: NodeBody>(&mut self, id: NodeID<T>) -> &mut AstNode<T>
    where
        T::Data: Into<<Root::Root as NodeDataStorage>::Storage>,
    {
        let node = &mut self.nodes[id.0];
        assert!(node.data.is_some());
        let node: &mut AstNode<T> = unsafe { std::mem::transmute(node) };
        node
    }

    pub fn walk_up<T: NodeBody>(
        &self,
        node_id: impl Into<NodeID<Root>>,
        test: impl Fn(&AstNode<Root>) -> Result<Option<NodeID<T>>, ()>,
    ) -> Result<Option<NodeID<T>>, ()>
    where
        NodeID<Root>: From<NodeID<<Root as NodeBody>::Root>>,
    {
        let mut node_id = node_id.into();
        loop {
            let node = self.get_ref(node_id);
            let result = test(node)?;

            if let Some(node_id) = result {
                break Ok(Some(node_id));
            } else if let Some(parent_id) = node.parent_id {
                node_id = parent_id.into();
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

pub struct AstNode<Body: NodeBody> {
    pub id: NodeID<Body>,
    pub parent_id: Option<NodeID<Body::Root>>,
    variant: Body::Variants,
    data: Option<<Body::Root as NodeDataStorage>::Storage>,
}

impl<Body: NodeBody> AstNode<Body> {
    pub fn variant(&self) -> &Body::Variants {
        &self.variant
    }

    pub fn get_ref(&self) -> AstNodeRef<Body> {
        AstNodeRef {
            id: self.id,
            parent_id: self.parent_id,
        }
    }

    pub fn body<'a>(&'a self) -> &'a Body::Data
    where
        &'a <Body::Root as NodeDataStorage>::Storage: TryInto<&'a Body::Data>,
        <&'a <Body::Root as NodeDataStorage>::Storage as TryInto<&'a Body::Data>>::Error: Debug,
        Body::Data: NodeData,
    {
        let storage: &<Body::Root as NodeDataStorage>::Storage = self.data.as_ref().unwrap();
        let body: &Body::Data = storage.try_into().unwrap();
        body
    }

    pub fn body_mut<'a>(&'a mut self) -> &'a mut Body::Data
    where
        &'a mut Body::Data: TryFrom<&'a mut <Body::Root as NodeDataStorage>::Storage>,
        <&'a mut <Body::Root as NodeDataStorage>::Storage as TryInto<&'a mut Body::Data>>::Error:
            Debug,
        Body::Data: NodeData,
    {
        let storage: &mut <Body::Root as NodeDataStorage>::Storage = self.data.as_mut().unwrap();
        let body: &mut Body::Data = storage.try_into().unwrap();
        body
    }
}
