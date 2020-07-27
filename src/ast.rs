use crate::token::Token;
use crate::token::{ArithmeticOP, TokenType};
use serde::export::Formatter;
use std::collections::VecDeque;
use std::iter::Peekable;
use std::{fmt, mem, result};

#[derive(Debug)]
pub struct Error {
    details: String,
}

impl Error {
    fn new(details: &str) -> Self {
        let details = format!("Ast Error: {}", details);
        panic!(details);
        // Error { details }
    }
}

type Result<N = NodeID> = result::Result<N, Error>;

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct NodeID(usize);

#[derive(Debug)]
pub struct Node {
    pub id: NodeID,
    pub parent_id: Option<NodeID>,
    pub tp: Option<NodeType>,
    pub tokens: Vec<Token>,
    pub body: NodeBody,
}

#[derive(Debug)]
pub enum NodeType {
    Void,
    Int,
}

#[derive(Debug)]
pub struct PendingNode {
    pub id: NodeID,
}

#[derive(Debug)]
pub enum NodeValue {
    Int(isize),
}

#[derive(Debug)]
pub enum NodeBody {
    Empty,
    Value(NodeValue),
    Op(ArithmeticOP, NodeID, NodeID),
    ProcedureDeclaration(Vec<NodeID>, NodeID),
    PrefixOp(ArithmeticOP, NodeID),
    Block(Vec<NodeID>),
    If(NodeID, NodeID),
    Loop(NodeID),
    Expression(NodeID),
    Comment(String),

    VariableDeclaration(String, Option<NodeID>),
    ConstDeclaration(String, Option<NodeID>),
    VariableAssignment(NodeID, NodeID),
    VariableValue(NodeID),
    Return(NodeID),
    Break(NodeID),
    Call(NodeID, Vec<NodeID>),

    Unlinked(UnlinkedNodeBody),
}

struct NodeBodyIterator<'a> {
    index: usize,
    body: &'a NodeBody,
    unlinked: Option<UnlinkedNodeBodyIterator<'a>>,
}

impl<'a> Iterator for NodeBodyIterator<'a> {
    type Item = &'a NodeID;

    fn next(&mut self) -> Option<&'a NodeID> {
        use NodeBody::*;
        let option = match self.body {
            Op(_, lhs, rhs) => match self.index {
                0 => Some(lhs),
                1 => Some(rhs),
                _ => None,
            },
            ProcedureDeclaration(args, body) => {
                if self.index < args.len() {
                    args.get(self.index)
                } else if self.index == args.len() {
                    Some(body)
                } else {
                    None
                }
            }
            PrefixOp(_, op) => match self.index {
                0 => Some(op),
                _ => None,
            },
            Block(children) => children.get(self.index),
            If(cond, body) => match self.index {
                0 => Some(cond),
                1 => Some(body),
                _ => None,
            },
            Loop(body) => match self.index {
                0 => Some(body),
                _ => None,
            },
            Expression(expr) => match self.index {
                0 => Some(expr),
                _ => None,
            },
            VariableDeclaration(_, value) => match self.index {
                0 => value.as_ref(),
                _ => None,
            },
            ConstDeclaration(_, value) => match self.index {
                0 => value.as_ref(),
                _ => None,
            },
            VariableAssignment(_, value) => match self.index {
                0 => Some(value),
                _ => None,
            },
            Call(_, args) => args.get(self.index),

            VariableValue(_) | Comment(_) | Return(_) | Break(_) | Value(_) | Empty => None,
            Unlinked(body) => {
                if let None = self.unlinked {
                    self.unlinked = Some(body.children());
                }
                match &mut self.unlinked {
                    Some(iter) => iter.next(),
                    None => unreachable!(),
                }
            }
        };
        if option.is_some() {
            self.index += 1;
        }
        option
    }
}

struct UnlinkedNodeBodyIterator<'a> {
    index: usize,
    body: &'a UnlinkedNodeBody,
}

impl<'a> Iterator for UnlinkedNodeBodyIterator<'a> {
    type Item = &'a NodeID;

    fn next(&mut self) -> Option<&'a NodeID> {
        use UnlinkedNodeBody::*;
        let option = match self.body {
            VariableAssignment(_, value) => match self.index {
                0 => Some(value),
                _ => None,
            },
            Call(_, args) => args.get(self.index),
            VariableValue(_) | Return | Break => None,
        };
        if option.is_some() {
            self.index += 1;
        }
        option
    }
}

impl NodeBody {
    fn children(&self) -> NodeBodyIterator {
        NodeBodyIterator {
            index: 0,
            body: self,
            unlinked: None,
        }
    }
}

#[derive(Debug)]
pub enum UnlinkedNodeBody {
    VariableAssignment(String, NodeID),
    VariableValue(String),
    Return,
    Break,
    Call(String, Vec<NodeID>),
}

impl UnlinkedNodeBody {
    fn children(&self) -> UnlinkedNodeBodyIterator {
        UnlinkedNodeBodyIterator {
            index: 0,
            body: self,
        }
    }
}

pub struct Ast {
    nodes: Vec<Node>,
    pub root: NodeID,
}

impl fmt::Debug for Ast {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.fmt_debug_node(f, 0, self.root)
    }
}

impl Ast {
    fn fmt_debug_node(&self, f: &mut Formatter<'_>, level: usize, node_id: NodeID) -> fmt::Result {
        let node = &self.nodes[node_id.0];
        let pad = " ".repeat(level * 2);
        let mut children = node.body.children().peekable();
        write!(f, "{}Node({}): {:?}", pad, node.id.0, node.body)?;
        if children.peek().is_some() {
            write!(f, " [\n")?;
            for &child in children {
                self.fmt_debug_node(f, level + 1, child)?;
                write!(f, "\n")?;
            }
            write!(f, "{}]", pad)?;
        }
        Ok(())
    }

    pub fn from_tokens<I>(iter: I) -> Result<Self>
    where
        I: Iterator<Item = Token>,
    {
        let parser = Parser::new(iter.peekable());
        let mut ast = parser.parse()?;
        let linker = Linker::new(&mut ast);
        linker.link()?;
        let typer = Typer::new(&mut ast);
        typer.infer_types()?;
        Ok(ast)
    }

    pub fn get_node(&self, node_id: NodeID) -> &Node {
        match self.nodes.get(node_id.0) {
            Some(node) => node,
            None => panic!("Could not find Node({}) in ast", node_id.0),
        }
    }

    pub fn get_node_mut(&mut self, node_id: NodeID) -> &mut Node {
        match self.nodes.get_mut(node_id.0) {
            Some(node) => node,
            None => panic!("Could not find Node({}) in ast", node_id.0),
        }
    }

    fn closest_fn(&self, node_id: NodeID) -> Result {
        let closest = self.closest(node_id, &|node| match node.body {
            NodeBody::ProcedureDeclaration(..) => Some(node.id),
            _ => None,
        });
        match closest {
            Some(id) => Ok(id),
            _ => Err(Error::new(&format!(
                "No function ancestor found starting at {:?}",
                node_id
            ))),
        }
    }

    fn closest_loop(&self, node_id: NodeID) -> Result {
        let closest = self.closest(node_id, &|node| match node.body {
            NodeBody::Loop(..) => Some(node.id),
            _ => None,
        });
        match closest {
            Some(id) => Ok(id),
            _ => Err(Error::new(&format!(
                "No loop ancestor found starting at {:?}",
                node_id
            ))),
        }
    }

    fn closest_variable(&self, node_id: NodeID, target_ident: &str) -> Result<NodeID> {
        use NodeBody::*;
        let closest = self.closest(node_id, &|node| {
            for &child_id in node.body.children() {
                let child = self.get_node(child_id);
                match &child.body {
                    VariableDeclaration(ident, _) | ConstDeclaration(ident, _) => {
                        if ident == target_ident {
                            return Some(child_id);
                        }
                    }
                    _ => (),
                }
            }
            None
        });
        match closest {
            Some(id) => Ok(id),
            _ => Err(Error::new(&format!(
                "Failed to find variable in scope starting at {:?}",
                node_id
            ))),
        }
    }

    fn closest(
        &self,
        mut node_id: NodeID,
        test: &dyn Fn(&Node) -> Option<NodeID>,
    ) -> Option<NodeID> {
        loop {
            let node = self.get_node(node_id);
            let result = test(node);
            if result.is_some() {
                break result;
            } else if let Some(id) = node.parent_id {
                node_id = id;
            } else {
                break None;
            }
        }
    }
}

struct Typer<'a> {
    queue: VecDeque<NodeID>,
    ast: &'a mut Ast,
}

impl<'a> Typer<'a> {
    fn new(ast: &'a mut Ast) -> Self {
        let queue = VecDeque::from(vec![ast.root]);
        Self { queue, ast }
    }

    fn infer_types(mut self) -> Result<()> {
        while let Some(node_id) = self.queue.pop_front() {
            let node = self.ast.get_node(node_id);
            for &child_id in node.body.children() {
                if let None = self.ast.get_node(child_id).tp {
                    self.queue.push_back(child_id)
                }
            }
        }
        Ok(())
    }
}

struct Linker<'a> {
    queue: VecDeque<NodeID>,
    ast: &'a mut Ast,
}

impl<'a> Linker<'a> {
    fn new(ast: &'a mut Ast) -> Self {
        let queue = VecDeque::from(vec![ast.root]);
        Self { queue, ast }
    }

    fn next(&mut self) -> Option<NodeID> {
        self.queue.pop_front()
    }

    fn link(mut self) -> Result<()> {
        while let Some(node_id) = self.next() {
            let node = self.ast.get_node(node_id);
            for &child in node.body.children() {
                self.queue.push_back(child);
            }
            match &node.body {
                NodeBody::Unlinked(body) => {
                    use UnlinkedNodeBody::*;
                    let new_body = match body {
                        VariableAssignment(ident, expr_id) => {
                            let target_id = self.ast.closest_variable(node_id, &ident)?;
                            NodeBody::VariableAssignment(target_id, *expr_id)
                        }
                        VariableValue(ident) => {
                            let target_id = self.ast.closest_variable(node_id, &ident)?;
                            NodeBody::VariableValue(target_id)
                        }
                        Return => {
                            let target_id = self.ast.closest_fn(node_id)?;
                            NodeBody::Return(target_id)
                        }
                        Break => {
                            let target_id = self.ast.closest_loop(node_id)?;
                            NodeBody::Break(target_id)
                        }
                        Call(ident, _) => {
                            let target_id = self.ast.closest_variable(node_id, &ident)?;
                            // We move the args out the old NodeBody so that we do not have to copy
                            // the argument vec.
                            let node = self.ast.get_node_mut(node_id);
                            let args = match &mut node.body {
                                NodeBody::Unlinked(Call(_, args)) => mem::replace(args, Vec::new()),
                                _ => unreachable!(),
                            };
                            NodeBody::Call(target_id, args)
                        }
                    };
                    let node = self.ast.get_node_mut(node_id);
                    node.body = new_body;
                }
                _ => (),
            }
        }
        Ok(())
    }
}

#[derive(Debug)]
struct Parser<I: Iterator<Item = Token>> {
    next_id: usize,
    nodes: Vec<Node>,
    iter: Peekable<I>,
}

impl<I> Parser<I>
where
    I: Iterator<Item = Token>,
{
    fn new(iter: Peekable<I>) -> Self {
        let next_id = 0;
        let nodes = Vec::new();
        Self {
            next_id,
            nodes,
            iter,
        }
    }

    fn token_precedence(token: &TokenType) -> usize {
        match token {
            TokenType::Op(op) => Self::op_precedence(*op),
            _ => 0,
        }
    }

    fn op_precedence(op: ArithmeticOP) -> usize {
        use crate::token::ArithmeticOP::*;
        match op {
            Eq => 1,
            Add | Sub => 2,
            Mul | Div => 3,
        }
    }

    fn ensure_next_token(&mut self, node: &mut PendingNode, tp: TokenType) -> Result<()> {
        self.ensure_next_token_for_id(node.id, tp)
    }

    fn next_token(&mut self, node: &mut PendingNode) -> Result<TokenType> {
        self.next_token_for_id(node.id)
    }

    fn ensure_next_token_for_id(&mut self, node_id: NodeID, tp: TokenType) -> Result<()> {
        let token = self.next_token_for_id(node_id);
        if token.is_err() {
            return Err(Error::new(&format!(
                "Expected token {:?} but got to the end of the file",
                tp
            )));
        }
        let token = token.unwrap();
        match token == tp {
            true => Ok(()),
            false => Err(Error::new(&format!(
                "Expected token {:?}, found {:?}",
                tp,
                self.nodes[node_id.0].tokens.last().unwrap()
            ))),
        }
    }

    fn next_token_for_id(&mut self, node_id: NodeID) -> Result<TokenType> {
        match self.iter.next() {
            Some(token) => {
                let tp = token.tp.clone();
                self.nodes[node_id.0].tokens.push(token);
                Ok(tp)
            }
            None => Err(Error::new("No more tokens")),
        }
    }

    fn peek_token(&mut self) -> Result<&TokenType> {
        match self.iter.peek() {
            Some(val) => Ok(&val.tp),
            None => Err(Error::new("No more tokens when peeking")),
        }
    }

    fn peek_token_or_none(&mut self) -> Option<&TokenType> {
        match self.iter.peek() {
            Some(val) => Some(&val.tp),
            None => None,
        }
    }

    fn node(&mut self, parent_id: NodeID) -> PendingNode {
        self.any_node(Some(parent_id))
    }

    fn root_node(&mut self) -> PendingNode {
        self.any_node(None)
    }

    fn any_node(&mut self, parent_id: Option<NodeID>) -> PendingNode {
        let id = NodeID(self.nodes.len());
        let empty = Node {
            id,
            parent_id,
            tp: None,
            body: NodeBody::Empty,
            tokens: Vec::new(),
        };
        self.nodes.push(empty);
        return PendingNode { id };
    }

    fn add_node(&mut self, pending: PendingNode, body: NodeBody) -> NodeID {
        self.nodes[pending.id.0].body = body;
        pending.id
    }

    fn add_uncomplete_node(&mut self, pending: PendingNode, body: UnlinkedNodeBody) -> NodeID {
        self.nodes[pending.id.0].body = NodeBody::Unlinked(body);
        pending.id
    }

    fn parse(mut self) -> Result<Ast> {
        let node = self.root_node();

        let mut statements = Vec::new();
        while self.iter.peek().is_some() {
            let statement = self.do_statement(node.id)?;
            statements.push(statement);
        }
        let root = self.add_node(node, NodeBody::Block(statements));
        Ok(Ast {
            root,
            nodes: self.nodes,
        })
    }

    fn should_terminate_statement(&mut self, node: NodeID) -> bool {
        use NodeBody::*;

        match self.nodes[node.0].body {
            ConstDeclaration(_, Some(child_node)) | VariableDeclaration(_, Some(child_node)) => {
                self.should_terminate_statement(child_node)
            }
            Block(..) | ProcedureDeclaration(..) | If(..) | Loop(..) | Comment(..) => false,
            _ => true,
        }
    }

    fn do_block(&mut self, mut node: PendingNode) -> Result {
        let mut statements = Vec::new();
        while *self.peek_token()? != TokenType::RightCurlyBrace {
            let statement = self.do_statement(node.id)?;
            statements.push(statement);
        }
        self.ensure_next_token(&mut node, TokenType::RightCurlyBrace)?;
        Ok(self.add_node(node, NodeBody::Block(statements)))
    }

    fn do_statement(&mut self, parent_id: NodeID) -> Result {
        use crate::token::TokenType::*;
        let mut node = self.node(parent_id);

        let node = match self.next_token(&mut node)? {
            Int(value) => Ok(self.add_node(node, NodeBody::Value(NodeValue::Int(value)))),
            Op(op) => self.do_operation(node, op, None),
            Name(symbol) => self.do_statement_symbol(node, &symbol),
            LeftCurlyBrace => self.do_block(node),
            KeyName(keyword) => match keyword.as_ref() {
                "return" => self.do_return(node),
                "if" => self.do_if(node),
                "loop" => self.do_loop(node),
                "break" => self.do_break(node),
                "continue" => unimplemented!(),
                keyword => Err(Error::new(&format!("Unknown keyword '{:?}'", keyword))),
            },
            Comment(comment) => Ok(self.add_node(node, NodeBody::Comment(comment))),
            other => Err(Error::new(&format!("Unknown token {:?}", other))),
        }?;
        if self.should_terminate_statement(node) {
            self.ensure_next_token_for_id(node, EndStatement)?;
        }
        Ok(node)
    }

    fn do_expression(&mut self, parent_id: NodeID) -> Result {
        use crate::token::TokenType::*;
        let mut node = self.node(parent_id);

        let node = match self.next_token(&mut node)? {
            Int(value) => self.add_node(node, NodeBody::Value(NodeValue::Int(value))),
            Op(op) => self.do_operation(node, op, None)?,
            Name(symbol) => self.do_expression_symbol(node, &symbol)?,
            LeftCurlyBrace => self.do_block(node)?,
            KeyName(key) if key == "fn" => self.do_procedure(node)?,
            LeftBrace => {
                let expr_node = self.do_expression(node.id)?;
                self.ensure_next_token(&mut node, RightBrace)?;
                self.add_node(node, NodeBody::Expression(expr_node))
            }
            other => Err(Error::new(&format!("Unkown token {:?}", other)))?,
        };

        match self.peek_token_or_none() {
            Some(TokenType::Op(op)) => {
                let op = *op;
                let mut op_node = self.node(node);
                self.next_token(&mut op_node)?;
                self.do_operation(op_node, op, Some(node))
            }
            _ => Ok(node),
        }
    }

    fn do_if(&mut self, mut node: PendingNode) -> Result {
        self.ensure_next_token(&mut node, TokenType::LeftBrace)?;
        let expr = self.do_expression(node.id)?;
        self.ensure_next_token(&mut node, TokenType::RightBrace)?;

        let mut body_node = self.node(node.id);
        self.ensure_next_token(&mut body_node, TokenType::LeftCurlyBrace)?;
        let body = self.do_block(body_node)?;
        Ok(self.add_node(node, NodeBody::If(expr, body)))
    }

    fn do_loop(&mut self, node: PendingNode) -> Result {
        let mut body_node = self.node(node.id);
        self.ensure_next_token(&mut body_node, TokenType::LeftCurlyBrace)?;
        let body = self.do_block(body_node)?;
        Ok(self.add_node(node, NodeBody::Loop(body)))
    }

    fn do_break(&mut self, node: PendingNode) -> Result {
        Ok(self.add_uncomplete_node(node, UnlinkedNodeBody::Break))
    }

    fn do_operation(
        &mut self,
        node: PendingNode,
        op: ArithmeticOP,
        pending_node: Option<NodeID>,
    ) -> Result {
        let body = {
            if let Some(lhs) = pending_node {
                // Operation between two nodes
                let rhs = self.do_expression(node.id)?;
                let next_token = self.peek_token()?;

                let lhs_precedence = Self::op_precedence(op);
                let rhs_precedence = Self::token_precedence(next_token);
                if lhs_precedence >= rhs_precedence {
                    NodeBody::Op(op, lhs, rhs)
                } else {
                    let mut node = self.node(node.id);
                    let rhs = match self.next_token(&mut node)? {
                        TokenType::Op(op) => self.do_operation(node, op, Some(rhs))?,
                        _ => Err(Error::new(&format!("Must be of type ")))?,
                    };
                    NodeBody::Op(op, lhs, rhs)
                }
            } else {
                match op {
                    ArithmeticOP::Add | ArithmeticOP::Sub => {
                        // Prefix operation of single node
                        let rhs = self.do_expression(node.id)?;
                        NodeBody::PrefixOp(op, rhs)
                    }
                    _ => Err(Error::new(&format!(
                        "Can only use prefix operations for addition and subtraction"
                    )))?,
                }
            }
        };
        Ok(self.add_node(node, body))
    }

    fn do_procedure(&mut self, mut node: PendingNode) -> Result {
        self.ensure_next_token(&mut node, TokenType::LeftBrace)?;
        let mut arguments = Vec::new();
        while self.peek_token()? != &TokenType::RightBrace {
            let mut arg_node = self.node(node.id);
            match self.next_token(&mut arg_node)? {
                TokenType::Name(name) => {
                    let body = self.add_node(arg_node, NodeBody::VariableDeclaration(name, None));
                    arguments.push(body)
                }
                _ => Err(Error::new(&format!(
                    "Invalid token found for procedure name"
                )))?,
            }

            if self.peek_token()? == &TokenType::ListSeparator {
                self.next_token(&mut node)?;
            } else {
                break;
            }
        }
        self.ensure_next_token(&mut node, TokenType::RightBrace)?;

        let mut block_node = self.node(node.id);
        self.ensure_next_token(&mut block_node, TokenType::LeftCurlyBrace)?;
        let block_node = self.do_block(block_node)?;

        Ok(self.add_node(node, NodeBody::ProcedureDeclaration(arguments, block_node)))
    }

    fn do_statement_symbol(&mut self, mut node: PendingNode, symbol: &str) -> Result {
        use crate::token::TokenType::*;

        let token = self.peek_token()?;
        match token {
            LeftBrace => self.do_function_call(node, symbol),
            StaticDeclaration => {
                self.next_token(&mut node)?;
                let expression = self.do_expression(node.id)?;
                let node = self.add_node(
                    node,
                    NodeBody::ConstDeclaration(symbol.into(), Some(expression)),
                );
                Ok(node)
            }
            Declaration => {
                self.next_token(&mut node)?;
                let expression = self.do_expression(node.id)?;
                let node = self.add_node(
                    node,
                    NodeBody::VariableDeclaration(symbol.into(), Some(expression)),
                );
                Ok(node)
            }
            Assignment => {
                self.next_token(&mut node)?;
                let expression = self.do_expression(node.id)?;
                let node = self.add_uncomplete_node(
                    node,
                    UnlinkedNodeBody::VariableAssignment(symbol.into(), expression),
                );
                Ok(node)
            }
            _ => Err(Error::new(&format!(
                "Unknown token: {:?} when parsing symbol with name {:?}",
                token, symbol
            ))),
        }
    }

    fn do_expression_symbol(&mut self, node: PendingNode, symbol: &str) -> Result {
        use crate::token::TokenType::*;

        let token = self.peek_token()?;
        match token {
            LeftBrace => self.do_function_call(node, symbol),
            Op(_) | RightBrace | EndStatement | ListSeparator => {
                Ok(self.add_uncomplete_node(node, UnlinkedNodeBody::VariableValue(symbol.into())))
            }
            _ => Err(Error::new(&format!(
                "Unkown token: {:?} when parsing symbol with name {:?}",
                token, symbol
            ))),
        }
    }

    fn do_function_call(&mut self, mut node: PendingNode, symbol: &str) -> Result {
        use crate::token::TokenType::*;

        self.ensure_next_token(&mut node, LeftBrace)?;
        let mut args = Vec::new();
        while self.peek_token()? != &RightBrace {
            let arg = self.do_expression(node.id)?;
            args.push(arg);
            if self.peek_token()? == &ListSeparator {
                self.next_token(&mut node)?;
            }
        }
        self.ensure_next_token(&mut node, RightBrace)?;
        Ok(self.add_uncomplete_node(node, UnlinkedNodeBody::Call(symbol.into(), args)))
    }

    fn do_return(&mut self, node: PendingNode) -> Result {
        Ok(self.add_uncomplete_node(node, UnlinkedNodeBody::Return))
    }
}
