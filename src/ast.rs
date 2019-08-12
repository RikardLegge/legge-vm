use crate::token::Token;
use crate::token::ArithmeticOp;
use std::iter::Peekable;
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
    ProcedureDeclaration(String, Vec<String>, Vec<AstNode>),
    ConstDeclaration(String, Box<AstNode>),
    Declaration(String, Box<AstNode>),
    Assignment(String, Box<AstNode>),
    GetVariable(String),
    PrefixOp(ArithmeticOp, Box<AstNode>),
    Scope(Vec<AstNode>),
    Return(Box<AstNode>),
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
    use crate::token::ArithmeticOp::*;
    match token {
        Token::Op(op) => match op {
            Add | Sub => 1,
            Mul | Div => 2
        },
        _ => 0
    }
}

fn ast_error(details: &str) -> AstError {
    panic!("{}", details);
}

impl<'a> TopDownAstParser<'a> {
    fn new(iter: &'a mut Peekable<IntoIter<Token>>) -> Self {
        let stack = Vec::new();
        let scope = Vec::new();
        let statement_tokens = Vec::new();
        TopDownAstParser { stack, scope, iter, statement_tokens }
    }

    fn next_token(&mut self) -> Result<Token, AstError> {
        match self.iter.next() {
            Some(val) => Ok(val),
            None => Err(ast_error("No more tokens"))
        }
    }

    fn peek_token(&mut self) -> Result<&Token, AstError> {
        match self.iter.peek() {
            Some(val) => Ok(val),
            None => Err(ast_error("No more tokens when peeking"))
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
                assert!(!self.has_token());
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
        use crate::token::Token::*;

        let old_stack = mem::replace(&mut self.stack, Vec::new());

        while self.has_token() {
            if *self.peek_token()? == RightCurlyBrace { break; }

            let statement = self.do_statement()?;

            match statement {
                AstNode::Scope(..) => (),
                AstNode::ProcedureDeclaration(..) => (),
                _ => match self.next_token()? {
                    EndStatement => (),
                    token => panic!("Token after statement was '{:?}', expecting ';'. The statement is {:?}", token, statement)
                }
            }

            self.stack.push(statement);
        }

        let stack = mem::replace(&mut self.stack, old_stack);
        let scope = AstNode::Scope(stack);
        Ok(scope)
    }

    fn do_statement(&mut self) -> AstResult {
        use crate::token::Token::*;
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
        use crate::token::Token::*;

        let token = self.next_token()?;
        let node = match token {
            Int(value) => AstNode::Primitive(value),
            Op(op) => self.do_operation(token, op)?,
            String(string) => AstNode::String(string),
            Name(symbol) => self.do_symbol(&symbol)?,
            LeftCurlyBrace => self.do_scope()?,
            LeftBrace => {
                let node = self.do_expression()?;
                assert_eq!(self.next_token()?, RightBrace);
                node
            }
            other => panic!("Unkown token {:?}", other)
        };

        match self.peek_token()? {
            Token::Op(op) => {
                let op = *op;
                self.push_stack(node);
                let token = self.next_token()?;
                Ok(self.do_operation(token, op)?)
            }
            _ => Ok(node)
        }
    }

    fn do_operation(&mut self, token: Token, op: ArithmeticOp) -> AstResult {
        match token {
            Token::Op(_) => (),
            _ => panic!("An opperation was not passed")
        }
        let node = {
            if let Ok(lhs) = self.pop_stack() {
                // Operation between two nodes
                let rhs = self.do_expression()?;
                let next_token = self.peek_token()?;

                let lhs_precedence = get_precedence(&token);
                let rhs_precedence = get_precedence(next_token);
                if lhs_precedence >= rhs_precedence {
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

    fn do_procedure(&mut self, symbol: &str) -> AstResult {
        if Token::KeyName("fn".to_string()) != self.next_token()? {panic!("This is not a procedure declaration")}
        assert_eq!(self.next_token()?, Token::LeftBrace);
        let mut arguments = Vec::new();
        while self.peek_token()? != &Token::RightBrace {
            let token = self.next_token()?;
            if let Token::Name(name) = token {
                arguments.push(name);
            } else {
                panic!("Invalid token");
            }

            if self.peek_token()? == &Token::ListSeparator {
                self.next_token()?;
            } else {
                break;
            }
        }

        assert_eq!(self.next_token()?, Token::RightBrace);
        assert_eq!(self.next_token()?, Token::LeftCurlyBrace);

        let scope = self.do_scope();

        if let AstNode::Scope(children) = scope? {
            Ok(AstNode::ProcedureDeclaration(symbol.to_string(), arguments, children))
        } else {
            panic!("Result of scope evaluation was not a scope");
        }
    }

    fn do_symbol(&mut self, symbol: &str) -> AstResult {
        use crate::token::Token::*;

        if symbol == "return" {
            let node = self.do_expression()?;
            return Ok(AstNode::Return(Box::new(node)));
        }

        let token = self.peek_token()?;
        let node = match token {
            LeftBrace => {
                self.next_token()?;
                let old_stack = mem::replace(&mut self.stack, Vec::new());
                while *self.peek_token()? != RightBrace {
                    let arg = self.do_expression()?;
                    self.push_stack(arg);
                    if self.peek_token()? == &ListSeparator {
                        self.next_token()?;
                    }
                }
                self.next_token()?;
                let args = mem::replace(&mut self.stack, old_stack);
                AstNode::Call(symbol.to_string(), args)
            }
            StaticDeclaration => {
                self.next_token()?;
                if let Token::KeyName(name) = self.peek_token()? {
                    match name.as_ref() {
                        "fn" => self.do_procedure(symbol)?,
                        _ => panic!("Unknown keyname")
                    }
                } else {
                    AstNode::ConstDeclaration(symbol.to_string(), Box::new(self.do_expression()?))
                }
            }
            Declaration => {
                self.next_token()?;
                AstNode::Declaration(symbol.to_string(), Box::new(self.do_expression()?))
            }
            Op(_) | RightBrace | EndStatement | ListSeparator => {
                AstNode::GetVariable(symbol.to_string())
            }
            Assignment => {
                self.next_token()?;
                AstNode::Assignment(symbol.to_string(), Box::new(self.do_expression()?))
            },
            _ => panic!("Unkown token: {:?} when parsing symbol with name {:?}", token, symbol)
        };
        Ok(node)
    }
}
