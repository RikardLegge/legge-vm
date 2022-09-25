use crate::ast_builder::AstBuilder;
use crate::node::AstNodeBody;
use crate::token::Token;
use crate::{try_cast_node, AstNode, Block, Error, Node, NodeID, NodeType, Result, Variable};
use std::fmt::{Debug, Formatter};
use std::marker::PhantomData;

pub fn from_tokens(tokens: Vec<Token>) -> Result<Ast> {
    AstBuilder::new(tokens.into_iter()).build()
}

pub struct Ast<T = Block, G = ()>
where
    T: Node,
{
    pub root: Option<NodeID<T>>,
    nodes: Vec<AstNode>,
    guarantees: PhantomData<fn() -> G>,
}

impl Debug for Ast<Block> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some(root) = self.root {
            write!(f, "Ast [\n")?;
            self.fmt_debug_node(f, 0, root.into())
        } else {
            write!(f, "Empty")
        }
    }
}

pub trait ValidAst {}

impl<T> Ast<T>
where
    T: Node,
{
    pub fn new() -> Self {
        Ast {
            root: None,
            nodes: vec![],
            guarantees: Default::default(),
        }
    }
}

impl Ast {
    fn fmt_debug_node(
        &self,
        f: &mut Formatter<'_>,
        level: usize,
        node_id: NodeID<()>,
    ) -> std::fmt::Result {
        if level > 100 {
            write!(f, "Nesting level too deep!")?;
            return Ok(());
        }
        let node = &self.nodes[node_id.id()];
        let mut children = node.body.as_ref().unwrap().children().peekable();

        let prefix = "";
        write!(f, "{}", prefix)?;

        let pad_len = 2 + level * 2;
        let pad_start =
            " ".repeat(std::cmp::max(0, pad_len as isize - prefix.len() as isize) as usize);
        let pad_end = " ".repeat(pad_len);
        write!(f, "{}", pad_start)?;

        write!(f, " {:?}", node.id.id())?;
        if let Ok(tp) = self.get_node_type(node.id) {
            write!(f, " : {:?}", tp)?;
        }
        if let Some(body) = &node.body {
            write!(f, " = {:?}", body)?;
        }

        if children.peek().is_some() {
            write!(f, " [\n")?;
            for &child in children {
                self.fmt_debug_node(f, level + 1, child)?;
                write!(f, "\n")?;
            }
            write!(f, " ")?;
            write!(f, "{}]", pad_end)?;
        }
        Ok(())
    }

    pub fn new_node<Child>(&mut self, parent_id: impl Into<NodeID>) -> NodeID<Child>
    where
        Child: Node,
    {
        self.new_node_internal(Some(parent_id))
    }

    pub fn new_root_node<Child>(&mut self) -> NodeID<Child>
    where
        Child: Node,
    {
        let parent: Option<NodeID> = None;
        self.new_node_internal(parent)
    }

    fn new_node_internal<Child>(&mut self, parent_id: Option<impl Into<NodeID>>) -> NodeID<Child>
    where
        Child: Node,
    {
        let index = self.nodes.len();
        let id = NodeID::<Child>::new(index);
        let node = AstNode {
            id: id.into(),
            parent_id: parent_id.map(|id| id.into()),
            body: None,
        };
        self.nodes.push(node);
        id
    }

    pub fn push<Child>(&mut self, id: NodeID<Child>, child: Child) -> NodeID<Child>
    where
        Child: Node,
    {
        let body = child.into();
        let node = self.get_mut(id);
        node.body = Some(body);
        id
    }

    pub fn closest_variable(
        &self,
        node_id: impl Into<NodeID>,
        target_ident: &str,
    ) -> Result<Option<NodeID<Variable>>> {
        self.walk_blocks_up(node_id, |node| {
            let block = node.body();
            if let Some(variable_id) = block.variables.get(target_ident) {
                return Ok(Some(*variable_id));
            }
            Ok(None)
        })
    }

    pub fn walk_blocks_up<F, NodeType>(
        &self,
        node_id: impl Into<NodeID>,
        test: F,
    ) -> Result<Option<NodeID<NodeType>>>
    where
        F: Fn(&AstNode<Block>) -> Result<Option<NodeID<NodeType>>>,
    {
        self.walk_up(node_id, |node| match try_cast_node!(node as Block) {
            Some(block) => test(block),
            None => Ok(None),
        })
    }

    pub fn walk_up<F, NodeType>(
        &self,
        node_id: impl Into<NodeID>,
        test: F,
    ) -> Result<Option<NodeID<NodeType>>>
    where
        F: Fn(&AstNode) -> Result<Option<NodeID<NodeType>>>,
    {
        let mut node_id = node_id.into();
        loop {
            let node = self.get(node_id);
            let result = test(node)?;
            match result {
                Some(node_id) => {
                    break Ok(Some(node_id));
                }
                _ => {
                    if let Some(parent_id) = node.parent_id {
                        node_id = parent_id;
                    } else {
                        break Ok(None);
                    }
                }
            }
        }
    }

    pub fn get(&self, node_id: impl Into<NodeID>) -> &AstNode {
        self.nodes.get(node_id.into().id()).unwrap()
    }

    pub fn get_node_type(&self, node_id: impl Into<NodeID>) -> Result<NodeType> {
        AstNode::node_type(node_id.into(), &self)
    }

    pub fn get_mut(&mut self, node_id: impl Into<NodeID>) -> &mut AstNode {
        self.nodes.get_mut(node_id.into().id()).unwrap()
    }

    pub fn get_inner<'a, Child>(&'a self, node_id: NodeID<Child>) -> &'a Child
    where
        &'a Child: TryFrom<&'a AstNodeBody>,
    {
        let node = self.nodes.get(node_id.id()).unwrap();
        let body: &AstNodeBody = node.body.as_ref().unwrap();
        let inner: &Child = body.try_into().map_err(|_| Error::InternalError).unwrap();
        inner
    }

    pub fn get_inner_mut<'a, Child>(&'a mut self, node_id: NodeID<Child>) -> &'a mut Child
    where
        &'a mut Child: TryFrom<&'a mut AstNodeBody>,
    {
        let node = self.nodes.get_mut(node_id.id()).unwrap();
        let body: &mut AstNodeBody = node.body.as_mut().unwrap();
        let inner: &mut Child = body.try_into().map_err(|_| Error::InternalError).unwrap();
        inner
    }
}
