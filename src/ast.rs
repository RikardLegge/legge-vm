use crate::token::Token;
use crate::token::{ArithmeticOP, TokenType};
use serde::export::Formatter;
use std::collections::VecDeque;
use std::iter::Peekable;
use std::vec::IntoIter;
use std::{fmt, result};

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

#[derive(Debug, Copy, Clone)]
pub struct NodeID(usize);

#[derive(Debug)]
pub struct Node {
    pub id: NodeID,
    pub parent_id: NodeID,
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
    pub parent_id: NodeID,
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

    VariableDeclaration(Option<NodeID>),
    ConstDeclaration(Option<NodeID>),
    VariableAssignment(NodeID, NodeID),
    VariableValue(NodeID),
    Return(NodeID),
    Break(NodeID),
    Call(NodeID, Vec<NodeID>),

    Unlinked(UnlinkedNodeBody),
}

impl NodeBody {
    fn children(&self) -> Vec<NodeID> {
        use NodeBody::*;
        match self {
            Op(_, lhs, rhs) => vec![*lhs, *rhs],
            ProcedureDeclaration(args, body) => {
                let mut children = Vec::with_capacity(args.len() + 1);
                children.append(&mut args.clone());
                children.push(*body);
                children
            }
            PrefixOp(_, op) => vec![*op],
            Block(children) => children.clone(),
            If(cond, body) => vec![*cond, *body],
            Loop(body) => vec![*body],
            Expression(expr) => vec![*expr],
            VariableDeclaration(value) => match value {
                Some(value) => vec![*value],
                None => vec![],
            },
            ConstDeclaration(value) => match value {
                Some(value) => vec![*value],
                None => vec![],
            },
            VariableAssignment(_, value) => vec![*value],
            Call(_, args) => args.clone(),
            Unlinked(unlinked) => unlinked.children(),

            VariableValue(_) | Comment(_) | Return(_) | Break(_) | Value(_) | Empty => vec![],
        }
    }
}

#[derive(Debug)]
pub enum UnlinkedNodeBody {
    VariableDeclaration(String, Option<NodeID>),
    ConstDeclaration(String, Option<NodeID>),
    VariableAssignment(String, NodeID),
    VariableValue(String),
    Return,
    Break,
    Call(String, Vec<NodeID>),
}

impl UnlinkedNodeBody {
    fn children(&self) -> Vec<NodeID> {
        use UnlinkedNodeBody::*;
        match self {
            VariableDeclaration(_, value) => match value {
                Some(value) => vec![*value],
                None => vec![],
            },
            ConstDeclaration(_, value) => match value {
                Some(value) => vec![*value],
                None => vec![],
            },
            VariableAssignment(_, value) => vec![*value],
            Call(_, args) => args.clone(),
            VariableValue(_) | Return | Break => vec![],
        }
    }
}

pub struct Ast {
    nodes: Vec<Node>,
    pub root: NodeID,
}

impl fmt::Debug for Ast {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.print_node(f, 0, self.root)
    }
}

impl Ast {
    fn print_node(&self, f: &mut Formatter<'_>, level: usize, node_id: NodeID) -> fmt::Result {
        let node = &self.nodes[node_id.0];
        let pad = " ".repeat(level * 2);
        let children = node.body.children();
        write!(f, "{}", pad)?;
        write!(f, "Node({}): {:?}", node.id.0, node.body)?;
        if children.is_empty() {
        } else {
            write!(f, "{{\n")?;
            for child in children {
                self.print_node(f, level + 1, child)?;
                write!(f, "\n")?;
            }
            write!(f, "{}}}", pad)?;
        }
        Ok(())
    }

    pub fn from_tokens(iter: &mut Peekable<IntoIter<Token>>) -> Result<Self> {
        let parser = Parser::new(iter);
        let mut ast = parser.parse()?;
        let linker = Linker::new(&mut ast);
        linker.link()?;
        Ok(ast)
    }

    fn get_node(&self, node_id: NodeID) -> &Node {
        &self.nodes[node_id.0]
    }

    fn get_node_mut(&mut self, node_id: NodeID) -> &mut Node {
        &mut self.nodes[node_id.0]
    }
}

struct Linker<'a> {
    pending_nodes: VecDeque<NodeID>,
    ast: &'a mut Ast,
}

impl<'a> Linker<'a> {
    fn new(ast: &'a mut Ast) -> Self {
        let mut pending_nodes = VecDeque::new();
        pending_nodes.push_back(ast.root);
        Linker { pending_nodes, ast }
    }

    fn link(mut self) -> Result<()> {
        while let Some(node_id) = self.pending_nodes.pop_front() {
            let node = self.ast.get_node(node_id);
            match &node.body {
                NodeBody::Unlinked(body) => {
                    println!("{:?}", body);
                }
                _ => {}
            }
            for child in node.body.children() {
                self.pending_nodes.push_back(child)
            }
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct Parser<'a> {
    next_id: usize,
    nodes: Vec<Node>,
    iter: &'a mut Peekable<IntoIter<Token>>,
}

fn get_precedence(token: &TokenType) -> usize {
    match token {
        TokenType::Op(op) => get_op_precedence(*op),
        _ => 0,
    }
}

fn get_op_precedence(op: ArithmeticOP) -> usize {
    use crate::token::ArithmeticOP::*;
    match op {
        Eq => 1,
        Add | Sub => 2,
        Mul | Div => 3,
    }
}

impl<'a> Parser<'a> {
    fn new(iter: &'a mut Peekable<IntoIter<Token>>) -> Self {
        let next_id = 0;
        let nodes = Vec::new();
        Parser {
            next_id,
            nodes,
            iter,
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

    fn node(&mut self, parent_id: NodeID) -> PendingNode {
        let id = NodeID(self.nodes.len());
        let empty = Node {
            id,
            parent_id,
            tp: None,
            body: NodeBody::Empty,
            tokens: Vec::new(),
        };
        self.nodes.push(empty);
        return PendingNode { id, parent_id };
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
        let node = self.node(NodeID(0));

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
            Unlinked(UnlinkedNodeBody::ConstDeclaration(_, Some(child_node)))
            | Unlinked(UnlinkedNodeBody::VariableDeclaration(_, Some(child_node))) => {
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

    fn do_statement(&mut self, parent_id: NodeID) -> Result<NodeID> {
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
            let token = self.next_token_for_id(node)?;
            if token != EndStatement {
                return Err(Error::new("Expected end of statement"));
            }
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

        match self.peek_token() {
            Ok(TokenType::Op(op)) => {
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

                let lhs_precedence = get_op_precedence(op);
                let rhs_precedence = get_precedence(next_token);
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
                    let body = self.add_uncomplete_node(
                        arg_node,
                        UnlinkedNodeBody::ConstDeclaration(name, None),
                    );
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
                let node = self.add_uncomplete_node(
                    node,
                    UnlinkedNodeBody::ConstDeclaration(symbol.into(), Some(expression)),
                );
                Ok(node)
            }
            Declaration => {
                self.next_token(&mut node)?;
                let expression = self.do_expression(node.id)?;
                let node = self.add_uncomplete_node(
                    node,
                    UnlinkedNodeBody::VariableDeclaration(symbol.into(), Some(expression)),
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
