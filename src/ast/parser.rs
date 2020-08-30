use super::{Ast, NodeBody, NodeID, NodeValue, Result, UnlinkedNodeBody};
use crate::token::TokenType::EndStatement;
use crate::token::{ArithmeticOP, Token, TokenType};
use std::iter::Peekable;

pub fn ast_from_tokens<I>(iter: I) -> Result<Ast>
where
    I: Iterator<Item = Token>,
{
    Parser::new(iter.peekable()).parse()
}

#[derive(Debug)]
struct PendingNode {
    pub id: NodeID,
}

#[derive(Debug)]
struct Parser<I: Iterator<Item = Token>> {
    ast: Ast,
    iter: Peekable<I>,
}

impl<I> Parser<I>
where
    I: Iterator<Item = Token>,
{
    fn new(iter: Peekable<I>) -> Self {
        let ast = Ast::new();
        Self { ast, iter }
    }

    fn parse(mut self) -> Result<Ast> {
        let node = self.any_node(None);

        let mut statements = Vec::new();
        while self.iter.peek().is_some() {
            let statement = self.do_statement(node.id)?;
            statements.push(statement);
        }
        self.add_node(node, NodeBody::Block(statements));
        Ok(self.ast)
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
            return Err(self.ast.error(
                &format!("Expected token {:?} but got to the end of the file", tp),
                "",
                vec![node_id],
            ));
        }
        let token = token.unwrap();
        match token == tp {
            true => Ok(()),
            false => Err(self.ast.error(
                &format!(
                    "Expected token {:?}, found {:?}",
                    tp,
                    self.ast.get_node(node_id).tokens.last().unwrap()
                ),
                "",
                vec![node_id],
            )),
        }
    }

    fn next_token_for_id(&mut self, node_id: NodeID) -> Result<TokenType> {
        match self.iter.next() {
            Some(token) => {
                let tp = token.tp.clone();
                self.ast.get_node_mut(node_id).tokens.push(token);
                Ok(tp)
            }
            None => Err(self.ast.error("No more tokens", "", vec![node_id])),
        }
    }

    fn peek_token(&mut self) -> Result<&TokenType> {
        match self.iter.peek() {
            Some(val) => Ok(&val.tp),
            None => Err(self.ast.error("No more tokens when peeking", "", vec![])),
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

    fn any_node(&mut self, parent_id: Option<NodeID>) -> PendingNode {
        let id = if let Some(parent_id) = parent_id {
            self.ast.add_node(parent_id)
        } else {
            self.ast.add_root_node()
        };
        return PendingNode { id };
    }

    fn add_node(&mut self, pending: PendingNode, body: NodeBody) -> NodeID {
        self.ast.get_node_mut(pending.id).body = body;
        pending.id
    }

    fn add_uncomplete_node(&mut self, pending: PendingNode, body: UnlinkedNodeBody) -> NodeID {
        self.ast.get_node_mut(pending.id).body = NodeBody::Unlinked(body);
        pending.id
    }

    fn should_terminate_statement(&mut self, node: NodeID) -> bool {
        use self::NodeBody::*;

        match self.ast.get_node(node).body {
            ConstDeclaration(.., child_node) | VariableDeclaration(.., Some(child_node)) => {
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
            Int(value) => Ok(self.add_node(node, NodeBody::ConstValue(NodeValue::Int(value)))),
            Op(op) => self.do_operation(node, op, None),
            Name(symbol) => self.do_statement_symbol(node, &symbol),
            LeftCurlyBrace => self.do_block(node),
            KeyName(keyword) => match keyword.as_ref() {
                "return" => self.do_return(node),
                "if" => self.do_if(node),
                "loop" => self.do_loop(node),
                "break" => self.do_break(node),
                "import" => self.do_import(node),
                "continue" => unimplemented!(),
                keyword => Err(self.ast.error(
                    &format!("Unknown keyword '{:?}'", keyword),
                    "",
                    vec![node.id],
                )),
            },
            Comment(comment) => Ok(self.add_node(node, NodeBody::Comment(comment))),
            other => Err(self
                .ast
                .error(&format!("Unknown token {:?}", other), "", vec![node.id])),
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
            Int(value) => self.add_node(node, NodeBody::ConstValue(NodeValue::Int(value))),
            String(value) => self.add_node(node, NodeBody::ConstValue(NodeValue::String(value))),
            Op(op) => self.do_operation(node, op, None)?,
            Name(symbol) => self.do_expression_symbol(node, &symbol)?,
            LeftCurlyBrace => self.do_block(node)?,
            KeyName(key) if key == "fn" => self.do_procedure(node)?,
            LeftBrace => {
                let expr_node = self.do_expression(node.id)?;
                self.ensure_next_token(&mut node, RightBrace)?;
                self.add_node(node, NodeBody::Expression(expr_node))
            }
            other => Err(self
                .ast
                .error(&format!("Unkown token {:?}", other), "", vec![node.id]))?,
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

    fn do_import(&mut self, mut node: PendingNode) -> Result {
        match self.next_token(&mut node)? {
            TokenType::Name(name) => {
                let body_node = self.node(node.id);
                let value = self.add_node(
                    body_node,
                    NodeBody::Unlinked(UnlinkedNodeBody::ImportValue(name.clone())),
                );
                Ok(self.add_node(node, NodeBody::Import(name, value)))
            }
            _ => unimplemented!(),
        }
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
                        _ => Err(self.ast.error(
                            &format!("Expecting operation, found something else"),
                            "",
                            vec![node.id],
                        ))?,
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
                    _ => Err(self.ast.error(
                        &format!("Can only use prefix operations for addition and subtraction"),
                        "",
                        vec![node.id],
                    ))?,
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
                    let tp = match self.peek_token()? {
                        TokenType::TypeDeclaration => {
                            self.next_token(&mut arg_node)?;
                            match self.next_token(&mut arg_node)? {
                                TokenType::Name(name) => Some(name),
                                _ => Err(self.ast.error(
                                    "A type name must come after ':' for procedure arguments",
                                    "",
                                    vec![arg_node.id],
                                ))?,
                            }
                        }
                        _ => None,
                    };
                    let body =
                        self.add_node(arg_node, NodeBody::VariableDeclaration(name, tp, None));
                    arguments.push(body)
                }
                _ => Err(self.ast.error(
                    &format!("Invalid token found for procedure name"),
                    "",
                    vec![arg_node.id],
                ))?,
            }

            if self.peek_token()? == &TokenType::ListSeparator {
                self.next_token(&mut node)?;
            } else {
                break;
            }
        }

        self.ensure_next_token(&mut node, TokenType::RightBrace)?;
        let returns = match self.peek_token()? {
            TokenType::ReturnTypes => {
                self.next_token(&mut node)?;
                match self.next_token(&mut node)? {
                    TokenType::Name(ident) => Some(ident),
                    token => panic!("Expected function type, found {:?}", token),
                }
            }
            _ => None,
        };

        let mut block_node = self.node(node.id);
        self.ensure_next_token(&mut block_node, TokenType::LeftCurlyBrace)?;
        let block_node = self.do_block(block_node)?;

        Ok(self.add_node(
            node,
            NodeBody::ProcedureDeclaration(arguments, returns, block_node),
        ))
    }

    fn do_statement_symbol(&mut self, mut node: PendingNode, ident: &str) -> Result {
        use crate::token::TokenType::*;

        match self.peek_token()? {
            LeftBrace => self.do_function_call(node, ident),
            TypeDeclaration => {
                self.next_token(&mut node)?;
                let token = self.next_token(&mut node)?;
                match token {
                    Name(tp) => {
                        let token = self.peek_token()?;
                        match token {
                            StaticDeclaration | VariableDeclaration => {
                                self.do_variable_or_assignment(node, ident, Some(tp))
                            }
                            EndStatement => {
                                let node = self.add_node(
                                    node,
                                    NodeBody::VariableDeclaration(ident.into(), Some(tp), None),
                                );
                                Ok(node)
                            }
                            _ => {
                                let details = &format!(
                                "Unexpected token {:?} after parsing variable type '{:?}' for '{:?}'. '::', ':=', or ';' expected",
                                token, tp, ident
                            );
                                Err(self.ast.error(details, "", vec![node.id]))
                            }
                        }
                    }
                    _ => Err(self.ast.error(
                        &format!(
                            "Unexpected token {:?} when parsing type name of ${:?}",
                            token, ident
                        ),
                        "",
                        vec![node.id],
                    )),
                }
            }
            _ => self.do_variable_or_assignment(node, ident, None),
        }
    }

    fn do_variable_or_assignment(
        &mut self,
        mut node: PendingNode,
        ident: &str,
        tp: Option<String>,
    ) -> Result {
        use crate::token::TokenType::*;

        let token = self.peek_token()?;
        match token {
            StaticDeclaration => {
                self.next_token(&mut node)?;
                let expression = self.do_expression(node.id)?;
                let node = self.add_node(
                    node,
                    NodeBody::ConstDeclaration(ident.into(), tp, expression),
                );
                Ok(node)
            }
            VariableDeclaration => {
                self.next_token(&mut node)?;
                let expression = self.do_expression(node.id)?;
                let node = self.add_node(
                    node,
                    NodeBody::VariableDeclaration(ident.into(), tp, Some(expression)),
                );
                Ok(node)
            }
            Assignment => {
                self.next_token(&mut node)?;
                let expression = self.do_expression(node.id)?;
                let node = self.add_uncomplete_node(
                    node,
                    UnlinkedNodeBody::VariableAssignment(ident.into(), expression),
                );
                Ok(node)
            }
            _ => {
                let details = &format!(
                    "Unexpected token: {:?} when parsing identifier with name {:?}",
                    token, ident
                );
                Err(self.ast.error(details, "", vec![node.id]))
            }
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
            _ => {
                let details = &format!(
                    "Unkown token: {:?} when parsing symbol with name {:?}",
                    token, symbol
                );
                Err(self.ast.error(details, "", vec![node.id]))
            }
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
        let ret = if self.peek_token()? != &EndStatement {
            let ret_id = self.do_expression(node.id)?;
            Some(ret_id)
        } else {
            None
        };
        Ok(self.add_uncomplete_node(node, UnlinkedNodeBody::Return(ret)))
    }
}
