use token::Token;
use std::iter::Peekable;
use token::ArithmeticOp;
use std::mem;
use std::vec::IntoIter;

#[derive(Debug)]
struct AstError {
    details: String
}

impl AstError {
    fn new(details: &str) -> AstError {
        AstError { details: details.to_string() }
    }
}

type AstResult = Result<AstNode, AstError>;

#[derive(Debug)]
pub enum AstNode {
    Primitive(i64),
    Op(ArithmeticOp, Box<AstNode>, Box<AstNode>),
    ConstDeclaration(String, Box<AstNode>),
    Declaration(String, Box<AstNode>),
    Assignment(String, Box<AstNode>),
    GetVariable(String),
    PrefixOp(ArithmeticOp, Box<AstNode>),
    Scope(Vec<AstNode>),
    Call(String, Vec<AstNode>),
    String(String),
}

#[derive(Debug)]
pub struct Ast {
    pub root: AstNode
}

impl Ast {
    pub fn from_tokens(iter: &mut Peekable<IntoIter<Token>>) -> Self {
        let parser = TopDownAstParser::new(iter);
        parser.parse()
    }
}

#[derive(Debug)]
pub struct TopDownAstParser<'a> {
    stack: Vec<AstNode>,
    scope: Vec<AstNode>,
    statement_tokens: Vec<Token>,
    iter: &'a mut Peekable<IntoIter<Token>>,
}

fn get_precedence(token: &Token) -> usize {
    use ::token::ArithmeticOp::*;
    match token {
        Token::Op(op) => match op {
            Add | Sub => 1,
            Mul | Div => 2
        },
        _ => 0
    }
}

impl<'a> TopDownAstParser<'a> {
    fn new(iter: &'a mut Peekable<IntoIter<Token>>) -> Self {
        let stack = Vec::new();
        let scope = Vec::new();
        let statement_tokens = Vec::new();
        TopDownAstParser { stack, scope, iter, statement_tokens }
    }

    fn next_token(&mut self) -> Result<Token, AstError> {
        if let Some(val) = self.iter.next() {
            Ok(val)
        } else {
            Err(AstError::new("No more tokens"))
        }
    }

    fn peek_token(&mut self) -> Result<&Token, AstError> {
        match self.iter.peek() {
            Some(val) => Ok(val),
            None => Err(AstError::new("No more tokens when peeking"))
        }
    }

    fn has_token(&mut self) -> bool {
        self.iter.peek().is_some()
    }

    fn pop_stack(&mut self) -> AstResult {
        match self.stack.pop() {
            Some(val) => Ok(val),
            None => Err(AstError::new("Stack empty, can not pop"))
        }
    }

    fn push_stack(&mut self, node: AstNode) {
        self.stack.push(node);
    }

    fn parse(mut self) -> Ast {
        match self.do_scope_content() {
            Ok(root) => {
                assert!(self.peek_token().is_err());
                assert_eq!(self.stack.len(), 0);
                Ast { root }
            }
            Err(e) => panic!("Ast error: {}", e.details)
        }
    }

    fn do_scope(&mut self) -> AstResult {
        let node = self.do_scope_content()?;
        assert_eq!(self.next_token()?, Token::RightCurlyBrace);
        Ok(node)
    }

    fn do_scope_content(&mut self) -> AstResult {
        use ::token::Token::*;

        while self.has_token() {
            if *self.peek_token()? == RightCurlyBrace { break; }

            let statement = self.do_statement()?;

            match statement {
                AstNode::Scope(..) => (),
                _ => match self.next_token()? {
                    EndStatement => (),
                    token => panic!("Token after statement was '{:?}', expecting ';'. The statement is {:?}", token, statement)
                }
            }

            self.stack.push(statement);
        }

        let stack = mem::replace(&mut self.stack, Vec::new());
        let scope = AstNode::Scope(stack);
        Ok(scope)
    }

    fn do_statement(&mut self) -> AstResult {
        use ::token::Token::*;
        assert_eq!(0, self.statement_tokens.len());

        let token = self.next_token()?;

        let node = match token {
            Int(value) => AstNode::Primitive(value),
            Op(op) => self.do_operation(token, op)?,
            String(string) => AstNode::String(string),
            Name(symbol) => self.do_symbol(&symbol)?,
            LeftCurlyBrace => self.do_scope()?,
            other => panic!("Unkown token {:?}", other)
        };

        self.statement_tokens.clear();
        Ok(node)
    }

    fn do_expression(&mut self) -> AstResult {
        use ::token::Token::*;

        let token = self.next_token()?;
        let node = match token {
            Int(value) => AstNode::Primitive(value),
            Op(op) => self.do_operation(token, op)?,
            String(string) => AstNode::String(string),
            Name(symbol) => self.do_symbol(&symbol)?,
            LeftCurlyBrace => self.do_scope()?,
            other => panic!("Unkown token {:?}", other)
        };
        Ok(node)
    }

    fn do_operation(&mut self, token: Token, op: ArithmeticOp) -> AstResult {
        let node = {
            if let Ok(lhs) = self.pop_stack() {
                // Operation between two nodes
                let rhs = self.do_expression()?;

                let lhs_precedence = get_precedence(&token);
                let rhs_precedence = get_precedence(self.peek_token()?);
                if lhs_precedence > rhs_precedence {
                    AstNode::Op(op, Box::new(lhs), Box::new(rhs))
                } else {
                    self.push_stack(rhs);
                    let rhs = self.do_expression()?;
                    AstNode::Op(op, Box::new(lhs), Box::new(rhs))
                }
            } else {
                // Prefix operation of single node
                let rhs = self.do_expression()?;
                AstNode::PrefixOp(op, Box::new(rhs))
            }
        };
        Ok(node)
    }

    fn do_symbol(&mut self, symbol: &str) -> AstResult {
        use ::token::Token::*;

        let token = self.peek_token()?;
        let node = match token {
            LeftBrace => {
                self.next_token()?;
                let old_stack = mem::replace(&mut self.stack, Vec::new());
                while *self.peek_token()? != RightBrace {
                    let arg = self.do_expression()?;
                    self.push_stack(arg);
                }
                self.next_token()?;
                let args = mem::replace(&mut self.stack, old_stack);
                AstNode::Call(symbol.to_string(), args)
            }
            StaticDeclaration => {
                self.next_token()?;
                AstNode::ConstDeclaration(symbol.to_string(), Box::new(self.do_expression()?))
            }
            Declaration => {
                self.next_token()?;
                AstNode::Declaration(symbol.to_string(), Box::new(self.do_expression()?))
            }
            Op(_) => {
                AstNode::GetVariable(symbol.to_string())
            }
            RightBrace => {
                AstNode::GetVariable(symbol.to_string())
            }
            Assignment => {
                self.next_token()?;
                AstNode::Assignment(symbol.to_string(), Box::new(self.do_expression()?))
            }
            _ => panic!("Unkown token: {:?}", token)
        };
        Ok(node)
    }
}
