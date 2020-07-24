use crate::ast::ParserScopeType::{Loop, Procedure};
use crate::token::Token;
use crate::token::{ArithmeticOp, TokenType};
use std::collections::LinkedList;
use std::iter::Peekable;
use std::mem;
use std::vec::IntoIter;

#[derive(Debug)]
struct AstError {
    details: String,
}

impl AstError {
    fn new(details: &str) -> AstError {
        AstError {
            details: details.to_string(),
        }
    }
}

type AstResult = Result<AstNode, AstError>;

pub type NodeID = usize;

#[derive(Debug)]
pub enum AssignmentType {
    ConstDeclaration,
    Declaration,
    Assignment,
}

#[derive(Debug)]
pub struct AstNode {
    pub id: NodeID,
    pub body: AstNodeBody,
}

#[derive(Debug)]
pub enum AstNodeBody {
    Primitive(i64),
    Op(ArithmeticOp, Box<AstNode>, Box<AstNode>),
    ProcedureDeclaration(String, Vec<String>, Option<String>, Vec<AstNode>),
    Assignment(AssignmentType, String, Box<AstNode>),
    GetVariable(String),
    PrefixOp(ArithmeticOp, Box<AstNode>),
    Block(Vec<AstNode>),
    Return(NodeID, Option<Box<AstNode>>),
    Break(NodeID),
    Call(String, Vec<AstNode>),
    If(Box<AstNode>, Box<AstNode>),
    Loop(Box<AstNode>),
    Comment(String),
}

#[derive(Debug)]
pub struct Ast {
    pub root: AstNode,
}

impl Ast {
    pub fn from_tokens(iter: &mut Peekable<IntoIter<Token>>) -> Self {
        let parser = TopDownAstParser::new(iter);
        parser.parse()
    }
}

#[derive(Debug)]
struct AstScope {
    list: LinkedList<ParserScope>,
}

impl AstScope {
    fn new() -> Self {
        AstScope {
            list: LinkedList::new(),
        }
    }

    fn closest_loop_id(&self) -> Option<NodeID> {
        for scope in self.list.iter() {
            if scope.tp == Loop {
                return Some(scope.node_id);
            }
        }
        None
    }

    fn closest_proc_id(&self) -> Option<NodeID> {
        for scope in self.list.iter() {
            if scope.tp == Procedure {
                return Some(scope.node_id);
            }
        }
        None
    }
}

#[derive(Debug)]
pub struct TopDownAstParser<'a> {
    pending_nodes: Vec<AstNode>,
    scope: AstScope,
    last_id: usize,
    iter: &'a mut Peekable<IntoIter<Token>>,
}

#[derive(Debug, PartialEq)]
enum ParserScopeType {
    Block,
    If,
    Loop,
    Procedure,
}

#[derive(Debug)]
struct ParserScope {
    node_id: NodeID,
    tp: ParserScopeType,
}

fn get_precedence(token: &TokenType) -> usize {
    use crate::token::ArithmeticOp::*;
    match token {
        TokenType::Op(op) => match op {
            Eq => 0,
            Add | Sub => 1,
            Mul | Div => 2,
        },
        _ => 0,
    }
}

fn ast_error(details: &str) -> AstError {
    panic!("{}", details);
}

impl<'a> TopDownAstParser<'a> {
    fn new(iter: &'a mut Peekable<IntoIter<Token>>) -> Self {
        let pending_nodes = Vec::new();
        let scope = AstScope::new();
        TopDownAstParser {
            pending_nodes,
            scope,
            last_id: 0,
            iter,
        }
    }

    fn next_token(&mut self) -> Result<TokenType, AstError> {
        match self.iter.next() {
            Some(val) => Ok(val.tp),
            None => Err(ast_error("No more tokens")),
        }
    }

    fn next_token_info(&mut self) -> Result<Token, AstError> {
        match self.iter.next() {
            Some(val) => Ok(val),
            None => Err(ast_error("No more tokens")),
        }
    }

    fn peek_token(&mut self) -> Result<&TokenType, AstError> {
        match self.iter.peek() {
            Some(val) => Ok(&val.tp),
            None => Err(ast_error("No more tokens when peeking")),
        }
    }

    fn has_token(&mut self) -> bool {
        self.iter.peek().is_some()
    }

    fn pop_stack(&mut self) -> AstResult {
        match self.pending_nodes.pop() {
            Some(val) => Ok(val),
            None => Err(AstError::new("Stack empty, can not pop")),
        }
    }

    fn push_stack(&mut self, node: AstNode) {
        self.pending_nodes.push(node);
    }

    fn next_id(&mut self) -> usize {
        let id = self.last_id + 1;
        self.last_id = id;
        id
    }

    fn push_scope(&mut self, tp: ParserScopeType) -> NodeID {
        let id = self.next_id();
        self.scope.list.push_front(ParserScope { node_id: id, tp });
        id
    }

    fn pop_scope(&mut self, tp: ParserScopeType) -> NodeID {
        let scope = self.scope.list.pop_front();
        if scope.is_none() {
            panic!("Expected scope type {:?} but no scope was available", tp);
        }
        let scope = scope.unwrap();
        if scope.tp != tp {
            panic!("Expected scope type {:?} but found {:?}", tp, scope.tp);
        }
        scope.node_id
    }

    fn node(&mut self, body: AstNodeBody) -> AstResult {
        let id = self.next_id();
        return Ok(AstNode { id, body });
    }

    fn scope_node(&mut self, id: NodeID, body: AstNodeBody) -> AstResult {
        return Ok(AstNode { id, body });
    }

    fn parse(mut self) -> Ast {
        match self.do_block_content() {
            Ok(root) => {
                assert!(!self.has_token());
                assert_eq!(self.pending_nodes.len(), 0);
                Ast { root }
            }
            Err(e) => panic!("Ast error: {}", e.details),
        }
    }

    fn do_block(&mut self) -> AstResult {
        let node = self.do_block_content()?;
        assert_eq!(self.next_token()?, TokenType::RightCurlyBrace);
        Ok(node)
    }

    fn do_block_content(&mut self) -> AstResult {
        use AstNodeBody::*;

        let old_stack = mem::replace(&mut self.pending_nodes, Vec::new());
        self.push_scope(ParserScopeType::Block);

        while self.has_token() {
            if *self.peek_token()? == TokenType::RightCurlyBrace {
                break;
            }

            let statement = self.do_statement()?;

            match statement.body {
                Block(..) | ProcedureDeclaration(..) | If(..) | Loop(..) | Comment(..) => (),
                _ => match self.next_token()? {
                    TokenType::EndStatement => (),
                    token => panic!(
                        "Token after statement was '{:?}', expecting ';'. The statement is {:?}",
                        token, statement
                    ),
                },
            }

            self.pending_nodes.push(statement);
        }

        let id = self.pop_scope(ParserScopeType::Block);
        let statements = mem::replace(&mut self.pending_nodes, old_stack);
        let body = Block(statements);
        self.scope_node(id, body)
    }

    fn do_statement(&mut self) -> AstResult {
        use crate::token::TokenType::*;

        let token = self.next_token_info()?;
        match token.tp {
            Int(value) => self.node(AstNodeBody::Primitive(value)),
            Op(op) => self.do_operation(token, op),
            Name(symbol) => self.do_statement_symbol(&symbol),
            LeftCurlyBrace => self.do_block(),
            KeyName(keyword) => match keyword.as_ref() {
                "return" => self.do_return(),
                "if" => self.do_if(),
                "loop" => self.do_loop(),
                "break" => self.do_break(),
                "continue" => unimplemented!(),
                keyword => panic!("Unknown keyword '{:?}'", keyword),
            },
            Comment(comment) => self.node(AstNodeBody::Comment(comment)),
            other => panic!("Unknown token {:?}", other),
        }
    }

    fn do_expression(&mut self) -> AstResult {
        use crate::token::TokenType::*;

        let token = self.next_token_info()?;
        let node = match token.tp {
            Int(value) => self.node(AstNodeBody::Primitive(value))?,
            Op(op) => self.do_operation(token, op)?,
            Name(symbol) => self.do_expression_symbol(&symbol)?,
            LeftCurlyBrace => self.do_block()?,
            LeftBrace => {
                let node = self.do_expression()?;
                assert_eq!(self.next_token()?, RightBrace);
                node
            }
            other => panic!("Unkown token {:?}", other),
        };

        match self.peek_token()? {
            TokenType::Op(op) => {
                let op = *op;
                self.push_stack(node);
                let token = self.next_token_info()?;
                self.do_operation(token, op)
            }
            _ => Ok(node),
        }
    }

    fn do_if(&mut self) -> AstResult {
        self.push_scope(ParserScopeType::If);
        assert_eq!(self.next_token()?, TokenType::LeftBrace);
        let expr = self.do_expression()?;
        assert_eq!(self.next_token()?, TokenType::RightBrace);

        assert_eq!(self.next_token()?, TokenType::LeftCurlyBrace);
        let body = self.do_block()?;
        let id = self.pop_scope(ParserScopeType::If);
        self.scope_node(id, AstNodeBody::If(Box::new(expr), Box::new(body)))
    }

    fn do_loop(&mut self) -> AstResult {
        assert_eq!(self.next_token()?, TokenType::LeftCurlyBrace);
        self.push_scope(ParserScopeType::Loop);
        let body = self.do_block()?;
        let id = self.pop_scope(ParserScopeType::Loop);
        self.scope_node(id, AstNodeBody::Loop(Box::new(body)))
    }

    fn do_break(&mut self) -> AstResult {
        let loop_id = match self.scope.closest_loop_id() {
            Some(id) => id,
            None => panic!("Break statement is not inside a loop"),
        };
        self.node(AstNodeBody::Break(loop_id))
    }

    fn do_operation(&mut self, token: Token, op: ArithmeticOp) -> AstResult {
        match token.tp {
            TokenType::Op(_) => (),
            _ => panic!("An operation was not passed"),
        }
        let node = {
            if let Ok(lhs) = self.pop_stack() {
                // Operation between two nodes
                let rhs = self.do_expression()?;
                let next_token = self.peek_token()?;

                let lhs_precedence = get_precedence(&token.tp);
                let rhs_precedence = get_precedence(next_token);
                if lhs_precedence >= rhs_precedence {
                    AstNodeBody::Op(op, Box::new(lhs), Box::new(rhs))
                } else {
                    self.push_stack(rhs);
                    let rhs = self.do_expression()?;
                    AstNodeBody::Op(op, Box::new(lhs), Box::new(rhs))
                }
            } else {
                match op {
                    ArithmeticOp::Add | ArithmeticOp::Sub => {
                        // Prefix operation of single node
                        let rhs = self.do_expression()?;
                        AstNodeBody::PrefixOp(op, Box::new(rhs))
                    }
                    _ => panic!("Can only use prefix operations for addition and subtraction"),
                }
            }
        };
        self.node(node)
    }

    fn do_procedure(&mut self, symbol: &str) -> AstResult {
        if TokenType::KeyName("fn".to_string()) != self.next_token()? {
            panic!("This is not a procedure declaration")
        }

        assert_eq!(self.next_token()?, TokenType::LeftBrace);
        let mut arguments = Vec::new();
        while self.peek_token()? != &TokenType::RightBrace {
            let token = self.next_token()?;
            if let TokenType::Name(name) = token {
                arguments.push(name);
            } else {
                panic!("Invalid token found for procedure name");
            }

            if self.peek_token()? == &TokenType::ListSeparator {
                self.next_token()?;
            } else {
                break;
            }
        }
        assert_eq!(self.next_token()?, TokenType::RightBrace);

        let mut return_type = None;
        if self.peek_token()? == &TokenType::ReturnTypes {
            self.next_token()?;
            if let TokenType::Name(_) = self.peek_token()? {
                if let TokenType::Name(var) = self.next_token()? {
                    return_type = Some(var);
                }
            } else {
                panic!("Missing return value type")
            }
        }

        assert_eq!(self.next_token()?, TokenType::LeftCurlyBrace);

        self.push_scope(ParserScopeType::Procedure);
        let scope = self.do_block();
        let id = self.pop_scope(ParserScopeType::Procedure);

        if let AstNodeBody::Block(children) = scope?.body {
            self.scope_node(
                id,
                AstNodeBody::ProcedureDeclaration(
                    symbol.to_string(),
                    arguments,
                    return_type,
                    children,
                ),
            )
        } else {
            panic!("Result of scope evaluation was not a scope");
        }
    }

    fn do_statement_symbol(&mut self, symbol: &str) -> AstResult {
        use crate::token::TokenType::*;

        let token = self.peek_token()?;
        match token {
            LeftBrace => self.do_function_call(symbol),
            StaticDeclaration => {
                self.next_token()?;
                if let TokenType::KeyName(name) = self.peek_token()? {
                    match name.as_ref() {
                        "fn" => self.do_procedure(symbol),
                        key => panic!("Unknown key name '{:?}'", key),
                    }
                } else {
                    let expression = self.do_expression()?;
                    self.node(AstNodeBody::Assignment(
                        AssignmentType::ConstDeclaration,
                        symbol.to_string(),
                        Box::new(expression),
                    ))
                }
            }
            Declaration => {
                self.next_token()?;
                let expression = self.do_expression()?;
                self.node(AstNodeBody::Assignment(
                    AssignmentType::Declaration,
                    symbol.to_string(),
                    Box::new(expression),
                ))
            }
            Assignment => {
                self.next_token()?;
                let expression = self.do_expression()?;
                self.node(AstNodeBody::Assignment(
                    AssignmentType::Assignment,
                    symbol.to_string(),
                    Box::new(expression),
                ))
            }
            _ => panic!(
                "Unknown token: {:?} when parsing symbol with name {:?}",
                token, symbol
            ),
        }
    }

    fn do_expression_symbol(&mut self, symbol: &str) -> AstResult {
        use crate::token::TokenType::*;

        let token = self.peek_token()?;
        match token {
            LeftBrace => self.do_function_call(symbol),
            Op(_) | RightBrace | EndStatement | ListSeparator => {
                self.node(AstNodeBody::GetVariable(symbol.to_string()))
            }
            _ => panic!(
                "Unkown token: {:?} when parsing symbol with name {:?}",
                token, symbol
            ),
        }
    }

    fn do_function_call(&mut self, symbol: &str) -> AstResult {
        use crate::token::TokenType::*;

        assert_eq!(self.next_token()?, LeftBrace);
        let old_stack = mem::replace(&mut self.pending_nodes, Vec::new());
        while self.peek_token()? != &RightBrace {
            let arg = self.do_expression()?;
            self.push_stack(arg);
            if self.peek_token()? == &ListSeparator {
                self.next_token()?;
            }
        }
        assert_eq!(self.next_token()?, RightBrace);
        let args = mem::replace(&mut self.pending_nodes, old_stack);
        self.node(AstNodeBody::Call(symbol.to_string(), args))
    }

    fn do_return(&mut self) -> AstResult {
        let proc_id = match self.scope.closest_proc_id() {
            Some(id) => id,
            None => panic!("Return statement is not inside a function"),
        };

        if self.peek_token()? != &TokenType::EndStatement {
            let node = self.do_expression()?;
            self.node(AstNodeBody::Return(proc_id, Some(Box::new(node))))
        } else {
            self.node(AstNodeBody::Return(proc_id, None))
        }
    }
}
