use crate::ast_builder::AstBuilder;
use crate::node::{AstRootNode, NodeUsage, Unknown};
use crate::token::Token;
use crate::{AstNode, Block, Error, Expression, Node, NodeID, NodeType, Result, Variable};
use std::fmt::{Debug, Formatter};
use std::marker::PhantomData;

pub fn from_tokens(tokens: Vec<Token>) -> Result<Ast> {
    AstBuilder::new(tokens.into_iter()).build()
}

#[derive(Debug, Clone, Copy)]
pub enum AstContext {
    Default,
    Chain(NodeID<Expression>),
}

pub struct Ast<T = Block, G = ()>
where
    T: Node,
{
    pub root: Option<NodeID<T>>,
    nodes: Vec<AstNode>,
    guarantees: PhantomData<fn() -> G>,
}

impl<T, G> Clone for Ast<T, G>
where
    T: Node,
{
    fn clone(&self) -> Self {
        Self {
            root: self.root,
            nodes: self.nodes.clone(),
            guarantees: self.guarantees,
        }
    }
}

impl Debug for Ast<Block> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some(root) = self.root {
            writeln!(f, "Ast [")?;
            self.fmt_debug_node(f, 0, root.into())
        } else {
            write!(f, "Empty")
        }
    }
}

pub trait ValidAst {}

impl<T> Default for Ast<T>
where
    T: Node,
{
    fn default() -> Self {
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
        node_id: NodeID<Unknown>,
    ) -> std::fmt::Result {
        if level > 100 {
            write!(f, "Nesting level too deep!")?;
            return Ok(());
        }
        let node = &self.nodes[node_id.id()];
        let mut children = node
            .body()
            .as_ref()
            .unwrap()
            .children(AstContext::Default)
            .peekable();

        let prefix = "";
        write!(f, "{}", prefix)?;

        let pad_len = 2 + level * 2;
        let pad_start =
            " ".repeat(std::cmp::max(0, pad_len as isize - prefix.len() as isize) as usize);
        let pad_end = " ".repeat(pad_len);
        write!(f, "{}", pad_start)?;

        write!(f, " {:?}", node.id.id())?;

        fn ignore_void(result: Result<NodeType>) -> Result<NodeType> {
            match result {
                Ok(NodeType::Void) => Err(Error::TypeNotInferred(NodeID::new(0))),
                other => other,
            }
        }
        let value_tp = ignore_void(self.get_node_type(node.id, NodeUsage::Value));
        let call_tp = ignore_void(self.get_node_type(node.id, NodeUsage::Call));
        let type_tp = ignore_void(self.get_node_type(node.id, NodeUsage::Type));
        match (&value_tp, &call_tp) {
            (Ok(value_tp), Ok(call_tp)) if value_tp == call_tp => write!(f, " : {:?}", value_tp)?,
            _ => {
                if let Ok(tp) = value_tp {
                    write!(f, " :value {:?}", tp)?;
                }
                if let Ok(tp) = call_tp {
                    write!(f, " :call {:?}", tp)?;
                }
            }
        }
        if let Ok(tp) = type_tp {
            write!(f, " :type {:?}", tp)?;
        }
        write!(f, " = {:?}", node.body())?;

        if children.peek().is_some() {
            writeln!(f, " [")?;
            for child in children {
                self.fmt_debug_node(f, level + 1, child.into())?;
                writeln!(f)?;
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
        let node = AstNode::new(id, parent_id);
        self.nodes.push(node);
        id
    }

    pub fn push<Child>(&mut self, id: NodeID<Child>, child: Child) -> NodeID<Child>
    where
        Child: Node + Into<AstRootNode>,
    {
        let body = child.into();
        let node = self.get_mut(id);
        *node.body_mut() = Some(body);
        id
    }

    pub fn push_new_node<Child>(
        &mut self,
        parent_id: impl Into<NodeID>,
        child: Child,
    ) -> NodeID<Child>
    where
        Child: Node + Into<AstRootNode>,
    {
        let id = self.new_node(parent_id.into());
        self.push(id, child)
    }

    pub fn closest_variable(
        &self,
        node_id: impl Into<NodeID>,
        target_ident: &str,
        context: AstContext,
    ) -> Result<Option<NodeID<Variable>>> {
        match context {
            AstContext::Default => {
                self.walk_blocks_up(node_id, |node| node.body().has_variable(target_ident))
            }
            AstContext::Chain(parent_id) => {
                let tp = self.get_node_type(parent_id, NodeUsage::Type)?;
                match tp {
                    NodeType::Custom(decl) => {
                        let body = self.get_body(decl);
                        body.has_variable(target_ident)
                    }
                    _ => Ok(None),
                }
            }
        }
    }

    pub fn walk_blocks_up<F, NodeType>(
        &self,
        node_id: impl Into<NodeID>,
        test: F,
    ) -> Result<Option<NodeID<NodeType>>>
    where
        F: Fn(&AstNode<Block>) -> Result<Option<NodeID<NodeType>>>,
    {
        self.walk_up(node_id, |node| match node.try_into().ok() {
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
        let node_id: NodeID = node_id.into();
        let index = node_id.id();
        let node = self.nodes.get(index);
        node.unwrap()
    }

    pub fn get_typed<Child>(&self, node_id: NodeID<Child>) -> &AstNode<Child>
    where
        Child: Node,
    {
        let node = self.get(node_id);
        // Safety: Since the node type Child was used to retrieve this element,
        // the underlying data must match this type.
        unsafe { std::mem::transmute(node) }
    }

    pub fn get_node_type(&self, node_id: impl Into<NodeID>, usage: NodeUsage) -> Result<NodeType> {
        match AstNode::node_type(node_id.into(), self, usage)? {
            NodeType::Indirect(target_id) => self.get_node_type(target_id, usage),
            tp => Ok(tp),
        }
    }

    pub fn get_mut(&mut self, node_id: impl Into<NodeID>) -> &mut AstNode {
        self.nodes.get_mut(node_id.into().id()).unwrap()
    }

    pub fn get_body<'a, Child>(&'a self, node_id: NodeID<Child>) -> &'a Child
    where
        &'a Child: TryFrom<&'a AstRootNode>,
    {
        let node = self.nodes.get(node_id.id()).unwrap();
        let body: &AstRootNode = node.body().as_ref().unwrap();
        let inner: &Child = body.try_into().map_err(|_| Error::InternalError).unwrap();
        inner
    }

    pub fn get_body_mut<'a, Child>(&'a mut self, node_id: NodeID<Child>) -> &'a mut Child
    where
        &'a mut Child: TryFrom<&'a mut AstRootNode>,
    {
        let node = self.nodes.get_mut(node_id.id()).unwrap();
        let body: &mut AstRootNode = node.body_mut().as_mut().unwrap();
        let inner: &mut Child = body.try_into().map_err(|_| Error::InternalError).unwrap();
        inner
    }
}
