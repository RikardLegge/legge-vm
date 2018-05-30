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
    fn new(details: &str) ->AstError {
        AstError {details: details.to_string()}
    }
}

type AstResult = Result<AstNode, AstError>;

#[derive(Debug, Clone)]
pub enum AstPrimitives {
    Int(i64),
    Str(String),
    Void,
}

#[derive(Debug)]
pub enum AstNode {
    Primitive(AstPrimitives),
    Op(ArithmeticOp, Box<AstNode>, Box<AstNode>),
    Scope(Vec<AstNode>),
    Call(String, Vec<AstNode>),
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
    iter: &'a mut Peekable<IntoIter<Token>>,
}

impl<'a> TopDownAstParser<'a> {
    fn new(iter: &'a mut Peekable<IntoIter<Token>>) -> Self {
        let stack = Vec::new();
        let scope = Vec::new();
        TopDownAstParser { stack, scope, iter }
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
        match self.do_scope() {
            Ok(root) => {
                assert!(self.peek_token().is_err());
                assert_eq!(self.stack.len(), 0);
                Ast { root }
            },
            Err(e) => panic!("Ast error: {}",e.details)
        }
    }

    fn do_scope(&mut self) -> AstResult {
        use ::token::Token::*;

        while self.has_token() {
            if *self.peek_token()? == RightCurlyBrace {break;}
            let expr = self.do_expression()?;
            self.stack.push(expr);
        }

        let stack = mem::replace(&mut self.stack, Vec::new());
        let scope = AstNode::Scope(stack);
        Ok(scope)
    }

    fn do_expression(&mut self) -> AstResult {
        use ::token::Token::*;

        let node = match self.next_token()? {
            Int(value) => AstNode::Primitive(AstPrimitives::Int(value)),
            Op(op) => {
                let lhs = self.pop_stack()?;
                let rhs = self.do_expression()?;
                AstNode::Op(op, Box::new(lhs), Box::new(rhs))
            }
            Name(symbol) => self.do_symbol(&symbol)?,
            other => panic!("Unkown token {:?}", other)
        };
        Ok(node)
    }

    fn do_symbol(&mut self, symbol: &str) -> AstResult {
        use ::token::Token::*;

        let token = self.next_token()?;
        let node = match token {
            LeftBrace => {
                let old_stack = mem::replace(&mut self.stack, Vec::new());
                while *self.peek_token()? != RightBrace {
                    let arg = self.do_expression()?;
                    self.push_stack(arg);
                }
                self.next_token()?;
                let args = mem::replace(&mut self.stack, old_stack);
                AstNode::Call(symbol.to_string(), args)
            },
            _ => panic!("Unkown token {:?}", token)
        };
        Ok(node)
    }
}
