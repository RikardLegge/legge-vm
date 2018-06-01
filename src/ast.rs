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
    StaticDeclaration(String, Box<AstNode>),
    Declaration(String, Box<AstNode>),
    GetVariable(String),
    PrefixOp(ArithmeticOp, Box<AstNode>),
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
            }
            Err(e) => panic!("Ast error: {}", e.details)
        }
    }

    fn do_scope(&mut self) -> AstResult {
        use ::token::Token::*;

        while self.has_token() {
            if *self.peek_token()? == RightCurlyBrace { break; }
            let expr = self.do_expression()?;
            self.stack.push(expr);
        }

        let stack = mem::replace(&mut self.stack, Vec::new());
        let scope = AstNode::Scope(stack);
        Ok(scope)
    }


    // 2+5*2-1 = 11
    // 5

    fn do_expression(&mut self) -> AstResult {
        use ::token::Token::*;

        let token = self.next_token()?;
        let node = match token {
            Int(value) => AstNode::Primitive(value),
            Op(op) => {
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
            }
            Name(symbol) => self.do_symbol(&symbol)?,
            other => panic!("Unkown token {:?}", other)
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
            },
            StaticDeclaration => {
                self.next_token()?;
                AstNode::StaticDeclaration(symbol.to_string(), Box::new(self.do_expression()?))
            },
            Declaration => {
                self.next_token()?;
                AstNode::Declaration(symbol.to_string(), Box::new(self.do_expression()?))
            }
            Op(_) => {
                AstNode::GetVariable(symbol.to_string())
            },
            RightBrace => {
                AstNode::GetVariable(symbol.to_string())
            },
            _ => panic!("Unkown token: {:?}", token)
        };
        Ok(node)
    }
}
