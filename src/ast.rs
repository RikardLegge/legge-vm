use crate::token::Token;
use crate::token::{ArithmeticOP, TokenType};
use std::iter::Peekable;
use std::vec::IntoIter;

#[derive(Debug)]
pub struct AstError {
    details: String,
}

impl AstError {
    fn new(details: &str) -> Self {
        let details = format!("Ast Error: {}", details);
        panic!(details);
        // AstError { details }
    }
}

type AstResult = Result<AstNode, AstError>;

#[derive(Debug)]
pub struct NodeID(usize);

#[derive(Debug)]
pub struct AstNode {
    pub id: NodeID,
    pub tokens: Vec<Token>,
    pub body: Option<AstNodeBody>,
}

impl AstNode {
    fn with_body(mut self, body: AstNodeBody) -> Self {
        self.body = Some(body);
        self
    }
}

#[derive(Debug)]
pub enum AstNodeValue {
    Int(isize),
}

#[derive(Debug)]
pub enum AstNodeBody {
    Value(AstNodeValue),
    Op(ArithmeticOP, Box<AstNode>, Box<AstNode>),
    ProcedureDeclaration(Vec<AstNode>, Box<AstNode>),
    VariableDeclaration(String, Option<Box<AstNode>>),
    ConstDeclaration(String, Option<Box<AstNode>>),
    VariableAssignment(String, Box<AstNode>),
    VariableValue(String),
    PrefixOp(ArithmeticOP, Box<AstNode>),
    Block(Vec<AstNode>),
    Return,
    Break,
    Call(String, Vec<AstNode>),
    If(Box<AstNode>, Box<AstNode>),
    Loop(Box<AstNode>),
    Expression(Box<AstNode>),
    Comment(String),
}

#[derive(Debug)]
pub struct Ast {
    pub root: AstNode,
}

impl Ast {
    pub fn from_tokens(iter: &mut Peekable<IntoIter<Token>>) -> Result<Self, AstError> {
        let parser = TopDownAstParser::new(iter);
        parser.parse()
    }
}

#[derive(Debug)]
pub struct TopDownAstParser<'a> {
    last_id: usize,
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

impl<'a> TopDownAstParser<'a> {
    fn new(iter: &'a mut Peekable<IntoIter<Token>>) -> Self {
        let last_id = 0;
        TopDownAstParser { last_id, iter }
    }

    fn ensure_next_token(&mut self, node: &mut AstNode, tp: TokenType) -> Result<(), AstError> {
        let token = self.next_token(node)?;
        match token == tp {
            true => Ok(()),
            false => Err(AstError::new(&format!(
                "Expected token '{:?}', found {:?}",
                tp, token
            ))),
        }
    }

    fn next_token(&mut self, node: &mut AstNode) -> Result<TokenType, AstError> {
        match self.iter.next() {
            Some(token) => {
                let tp = token.tp.clone();
                node.tokens.push(token);
                Ok(tp)
            }
            None => Err(AstError::new("No more tokens")),
        }
    }

    fn peek_token(&mut self) -> Result<&TokenType, AstError> {
        match self.iter.peek() {
            Some(val) => Ok(&val.tp),
            None => Err(ast_error("No more tokens when peeking")),
        }
    }

    fn next_id(&mut self) -> usize {
        let id = self.last_id + 1;
        self.last_id = id;
        id
    }

    fn node(&mut self) -> AstNode {
        let id = NodeID(self.next_id());
        let tokens = Vec::new();
        let body = None;
        return AstNode { id, tokens, body };
    }

    fn parse(mut self) -> Result<Ast, AstError> {
        let node = self.node();

        let mut statements = Vec::new();
        while self.iter.peek().is_some() {
            let statement = self.do_statement()?;
            statements.push(statement);
        }
        let root = node.with_body(AstNodeBody::Block(statements));
        Ok(Ast { root })
    }

    fn do_block(&mut self, node: AstNode) -> AstResult {
        let mut node = self.do_block_content(node)?;
        self.ensure_next_token(&mut node, TokenType::RightCurlyBrace)?;
        Ok(node)
    }

    fn do_block_content(&mut self, node: AstNode) -> AstResult {
        let mut statements = Vec::new();
        while *self.peek_token("expecting }")? != TokenType::RightCurlyBrace {
            let statement = self.do_statement()?;
            statements.push(statement);
        }
        Ok(node.with_body(AstNodeBody::Block(statements)))
    }

    fn should_terminate_statement(&mut self, node: &mut AstNode) -> Result<bool, AstError> {
        use AstNodeBody::*;

        if let Some(body) = &mut node.body {
            match body {
                Block(..) | ProcedureDeclaration(..) | If(..) | Loop(..) | Comment(..) => Ok(false),
                ConstDeclaration(_, Some(child_node))
                | VariableDeclaration(_, Some(child_node)) => {
                    self.should_terminate_statement(&mut *child_node)
                }
                _ => match self.peek_token("expecting ;")? {
                    TokenType::EndStatement => Ok(true),
                    _ => Ok(false),
                },
            }
        } else {
            Err(AstError::new(&format!("Statement is missing body")))
        }
    }

    fn do_statement(&mut self) -> AstResult {
        use crate::token::TokenType::*;
        let mut node = self.node();

        let mut node = match self.next_token(&mut node)? {
            Int(value) => Ok(node.with_body(AstNodeBody::Value(AstNodeValue::Int(value)))),
            Op(op) => self.do_operation(node, op, None),
            Name(symbol) => self.do_statement_symbol(node, &symbol),
            LeftCurlyBrace => self.do_block(node),
            KeyName(keyword) => match keyword.as_ref() {
                "return" => self.do_return(node),
                "if" => self.do_if(node),
                "loop" => self.do_loop(node),
                "break" => self.do_break(node),
                "continue" => unimplemented!(),
                keyword => Err(AstError::new(&format!("Unknown keyword '{:?}'", keyword))),
            },
            Comment(comment) => Ok(node.with_body(AstNodeBody::Comment(comment))),
            other => Err(AstError::new(&format!("Unknown token {:?}", other))),
        }?;
        if self.should_terminate_statement(&mut node)? {
            self.ensure_next_token(&mut node, EndStatement)?;
        }
        Ok(node)
    }

    fn do_expression(&mut self) -> AstResult {
        use crate::token::TokenType::*;
        let mut node = self.node();

        let node = match self.next_token(&mut node)? {
            Int(value) => node.with_body(AstNodeBody::Value(AstNodeValue::Int(value))),
            Op(op) => self.do_operation(node, op, None)?,
            Name(symbol) => self.do_expression_symbol(node, &symbol)?,
            LeftCurlyBrace => self.do_block(node)?,
            KeyName(key) if key == "fn" => self.do_procedure(node)?,
            LeftBrace => {
                let expr_node = self.do_expression()?;
                self.ensure_next_token(&mut node, RightBrace)?;
                node.with_body(AstNodeBody::Expression(Box::new(expr_node)))
            }
            other => Err(AstError::new(&format!("Unkown token {:?}", other)))?,
        };

        match self.peek_token() {
            Some(TokenType::Op(op)) => {
                let op = *op;
                let mut op_node = self.node();
                self.next_token(&mut op_node)?;
                self.do_operation(op_node, op, Some(node))
            }
            _ => Ok(node),
        }
    }

    fn do_if(&mut self, mut node: AstNode) -> AstResult {
        self.ensure_next_token(&mut node, TokenType::LeftBrace)?;
        let expr = self.do_expression()?;
        self.ensure_next_token(&mut node, TokenType::RightBrace)?;

        let mut body_node = self.node();
        self.ensure_next_token(&mut body_node, TokenType::LeftCurlyBrace)?;
        let body = self.do_block(body_node)?;
        Ok(node.with_body(AstNodeBody::If(Box::new(expr), Box::new(body))))
    }

    fn do_loop(&mut self, node: AstNode) -> AstResult {
        let mut body_node = self.node();
        self.ensure_next_token(&mut body_node, TokenType::LeftCurlyBrace)?;
        let body = self.do_block(body_node)?;
        Ok(node.with_body(AstNodeBody::Loop(Box::new(body))))
    }

    fn do_break(&mut self, node: AstNode) -> AstResult {
        Ok(node.with_body(AstNodeBody::Break))
    }

    fn do_operation(
        &mut self,
        node: AstNode,
        op: ArithmeticOP,
        pending_node: Option<AstNode>,
    ) -> AstResult {
        let body = {
            if let Some(lhs) = pending_node {
                // Operation between two nodes
                let rhs = self.do_expression()?;
                let next_token = self.peek_token()?;

                let lhs_precedence = get_op_precedence(op);
                let rhs_precedence = get_precedence(next_token);
                if lhs_precedence >= rhs_precedence {
                    AstNodeBody::Op(op, Box::new(lhs), Box::new(rhs))
                } else {
                    let mut node = self.node();
                    let rhs = match self.next_token(&mut node)? {
                        TokenType::Op(op) => self.do_operation(node, op, Some(rhs))?,
                        _ => Err(AstError::new(&format!("Must be of type ")))?,
                    };
                    AstNodeBody::Op(op, Box::new(lhs), Box::new(rhs))
                }
            } else {
                match op {
                    ArithmeticOP::Add | ArithmeticOP::Sub => {
                        // Prefix operation of single node
                        let rhs = self.do_expression()?;
                        AstNodeBody::PrefixOp(op, Box::new(rhs))
                    }
                    _ => Err(AstError::new(&format!(
                        "Can only use prefix operations for addition and subtraction"
                    )))?,
                }
            }
        };
        Ok(node.with_body(body))
    }

    fn do_procedure(&mut self, mut node: AstNode) -> AstResult {
        self.ensure_next_token(&mut node, TokenType::LeftBrace)?;
        let mut arguments = Vec::new();
        while self.peek_token()? != &TokenType::RightBrace {
            let mut arg_node = self.node();
            match self.next_token(&mut arg_node)? {
                TokenType::Name(name) => {
                    arguments.push(arg_node.with_body(AstNodeBody::ConstDeclaration(name, None)))
                }
                _ => Err(AstError::new(&format!(
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

        let mut block_node = self.node();
        self.ensure_next_token(&mut block_node, TokenType::LeftCurlyBrace)?;
        let block_node = self.do_block(block_node)?;

        Ok(node.with_body(AstNodeBody::ProcedureDeclaration(
            arguments,
            Box::new(block_node),
        )))
    }

    fn do_statement_symbol(&mut self, mut node: AstNode, symbol: &str) -> AstResult {
        use crate::token::TokenType::*;

        let token = self.peek_token()?;
        match token {
            LeftBrace => self.do_function_call(node, symbol),
            StaticDeclaration => {
                self.next_token(&mut node)?;
                let expression = self.do_expression()?;
                let node = node.with_body(AstNodeBody::ConstDeclaration(
                    symbol.into(),
                    Some(Box::new(expression)),
                ));
                Ok(node)
            }
            Declaration => {
                self.next_token(&mut node)?;
                let expression = self.do_expression()?;
                let node = node.with_body(AstNodeBody::VariableDeclaration(
                    symbol.into(),
                    Some(Box::new(expression)),
                ));
                Ok(node)
            }
            Assignment => {
                self.next_token(&mut node)?;
                let expression = self.do_expression()?;
                let node = node.with_body(AstNodeBody::VariableAssignment(
                    symbol.into(),
                    Box::new(expression),
                ));
                Ok(node)
            }
            _ => Err(AstError::new(&format!(
                "Unknown token: {:?} when parsing symbol with name {:?}",
                token, symbol
            ))),
        }
    }

    fn do_expression_symbol(&mut self, node: AstNode, symbol: &str) -> AstResult {
        use crate::token::TokenType::*;

        let token = self.peek_token()?;
        match token {
            LeftBrace => self.do_function_call(node, symbol),
            Op(_) | RightBrace | EndStatement | ListSeparator => {
                Ok(node.with_body(AstNodeBody::VariableValue(symbol.into())))
            }
            _ => Err(AstError::new(&format!(
                "Unkown token: {:?} when parsing symbol with name {:?}",
                token, symbol
            ))),
        }
    }

    fn do_function_call(&mut self, mut node: AstNode, symbol: &str) -> AstResult {
        use crate::token::TokenType::*;

        self.ensure_next_token(&mut node, LeftBrace)?;
        let mut args = Vec::new();
        while self.peek_token()? != &RightBrace {
            let arg = self.do_expression()?;
            args.push(arg);
            if self.peek_token()? == &ListSeparator {
                self.next_token(&mut node)?;
            }
        }
        self.ensure_next_token(&mut node, RightBrace)?;
        Ok(node.with_body(AstNodeBody::Call(symbol.into(), args)))
    }

    fn do_return(&mut self, node: AstNode) -> AstResult {
        Ok(node.with_body(AstNodeBody::Return))
    }
}
