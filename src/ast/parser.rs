use super::{Ast, NodeID, NodeValue, Result};
use crate::ast;
use crate::ast::ast::{PartialNodeValue, PartialType, ProcedureDeclarationNode};
use crate::ast::nodebody::{NBProcedureDeclaration, NodeBody, UnlinkedNodeBody};
use crate::ast::NodeType;
use crate::token::TokenType::EndStatement;
use crate::token::{ArithmeticOP, Token, TokenType};
use colored::Colorize;
use std::iter::Peekable;

pub fn ast_from_tokens<I>(iter: I) -> Result<Ast>
where
    I: Iterator<Item = Token>,
{
    Parser::new(iter).parse()
}

#[derive(Debug)]
struct PendingNode {
    pub id: NodeID,
}

#[derive(Debug)]
struct Parser<I: Iterator<Item = Token>> {
    ast: Ast,
    iter: Peekable<I>,
    pending_statement: Option<NodeID>,
    pending_expression: Option<NodeID>,
}

impl<I> Parser<I>
where
    I: Iterator<Item = Token>,
{
    fn new(iter: I) -> Self {
        let ast = Ast::new();

        Self {
            ast,
            iter: iter.peekable(),
            pending_expression: None,
            pending_statement: None,
        }
    }

    fn parse(mut self) -> Result<Ast> {
        let node = self.any_node(None);

        let mut static_statements = Vec::new();
        let mut dynamic_statements = Vec::new();
        while self.peek_token().is_ok() {
            let statement = self.do_statement(node.id)?;
            match self.ast.get_node(statement).body {
                NodeBody::TypeDeclaration { .. }
                | NodeBody::StaticDeclaration { .. }
                | NodeBody::Import { .. } => static_statements.push(statement),
                _ => dynamic_statements.push(statement),
            }
        }
        let statements = {
            static_statements.append(&mut dynamic_statements);
            static_statements
        };
        self.add_node(node, NodeBody::Block { body: statements });
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
            Eq | GEq | LEq => 1,
            Add | Sub => 2,
            Mul | Div => 3,
        }
    }

    fn ensure_next_token(&mut self, node: &PendingNode, tp: TokenType) -> Result<()> {
        self.ensure_next_token_for_id(node.id, tp, "")
    }

    fn next_token(&mut self, node: &PendingNode) -> Result<TokenType> {
        self.next_token_for_id(node.id)
    }

    fn pending_nodes(ast: &Ast, node_id: Option<NodeID>) -> Vec<NodeID> {
        match node_id {
            Some(node_id) => ast.nodes_after(node_id),
            None => vec![],
        }
    }

    fn ensure_next_token_for_id(
        &mut self,
        node_id: NodeID,
        tp: TokenType,
        error_description: &str,
    ) -> Result<()> {
        let token = self.next_token_for_id(node_id);
        let token = match token {
            Ok(token) => token,
            Err(_) => {
                return Err(self.ast.single_error(
                    &format!(
                        "Expected {} but got to the end of the file",
                        tp.to_string().red()
                    ),
                    &format!("Add {}", format!("{}", tp).red()),
                    Self::pending_nodes(&self.ast, self.pending_expression),
                ))
            }
        };
        if token != tp {
            let last = self.ast.get_node_mut(node_id).tokens.pop().unwrap();
            let error_node = self.any_node(Some(node_id));
            let error_node_id = self.add_node(error_node, NodeBody::Empty);
            self.ast.get_node_mut(error_node_id).tokens.push(last);
            return if tp == EndStatement {
                Err(self.ast.single_error(
                    &format!("The statement is never closed. Add ;"),
                    error_description,
                    self.ast
                        .get_node_and_children(self.pending_statement.unwrap()),
                ))
            } else {
                Err(self.ast.single_error(
                    &format!("Expected token '{}', found '{}'", tp, token,),
                    error_description,
                    vec![error_node_id],
                ))
            };
        }
        Ok(())
    }

    fn next_token_for_id(&mut self, node_id: NodeID) -> Result<TokenType> {
        match self.iter.next() {
            Some(token) => {
                let tp = token.tp.clone();
                self.ast.get_node_mut(node_id).tokens.push(token);
                Ok(tp)
            }
            None => Err(Self::no_more_tokens_error(
                self.pending_statement,
                &self.ast,
            )),
        }
    }

    fn no_more_tokens_error(node_id: Option<NodeID>, ast: &Ast) -> ast::Err {
        let mut nodes = vec![];
        if let Some(id) = node_id {
            nodes.push(id);
            let last_node_id = ast.nodes().last().unwrap().id();
            if last_node_id != id {
                nodes.push(last_node_id);
            }
        }
        ast.single_error("No more tokens", "Incomplete statement", nodes)
    }

    fn peek_token(&mut self) -> Result<&TokenType> {
        match self.iter.peek() {
            Some(val) => Ok(&val.tp),
            None => Err(Self::no_more_tokens_error(
                self.pending_statement,
                &self.ast,
            )),
        }
    }

    fn missing_token_error(ast: &Ast, tt: &TokenType, node_id: Option<NodeID>) -> ast::Err {
        ast.single_error(
            &format!(
                "Reached en of file while looking for {}",
                tt.to_string().red()
            ),
            &format!("Add {}", tt.to_string().red()),
            Self::pending_nodes(&ast, node_id),
        )
    }

    fn wrong_token_error(
        ast: &Ast,
        wanted: &TokenType,
        got: &TokenType,
        node_id: Option<NodeID>,
    ) -> ast::Err {
        ast.single_error(
            &format!(
                "Found an unexpected token, consider replacing {} with {}",
                got.to_string().red(),
                wanted.to_string().red(),
            ),
            &format!("Replace with {}", wanted.to_string().red()),
            Self::pending_nodes(&ast, node_id),
        )
    }

    fn peek_token_not(&mut self, tt: &TokenType) -> Result<Option<&TokenType>> {
        match self.iter.peek() {
            Some(token) if tt != &token.tp => Ok(Some(&token.tp)),
            Some(_) => Ok(None),
            None => {
                let node_id = self.pending_expression;
                Err(Self::missing_token_error(&self.ast, tt, node_id))
            }
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

    fn add_node(&mut self, pending_node: PendingNode, body: NodeBody) -> NodeID {
        let node = self.ast.get_node_mut(pending_node.id);
        node.body = body;
        pending_node.id
    }

    fn add_uncomplete_node(&mut self, pending: PendingNode, body: UnlinkedNodeBody) -> NodeID {
        self.ast.get_node_mut(pending.id).body = NodeBody::Unlinked(body);
        pending.id
    }

    fn should_terminate_statement(&mut self, node: NodeID) -> bool {
        use crate::ast::nodebody::NodeBody::*;

        match self.ast.get_node(node).body {
            TypeDeclaration { constructor, .. } => self.should_terminate_statement(*constructor),
            ConstDeclaration { expr, .. }
            | StaticDeclaration { expr, .. }
            | VariableDeclaration {
                expr: Some(expr), ..
            } => self.should_terminate_statement(expr),
            Block { .. }
            | ProcedureDeclaration(NBProcedureDeclaration { .. })
            | If { .. }
            | Loop { .. }
            | Comment { .. }
            | Empty => false,
            _ => true,
        }
    }

    fn do_block(&mut self, node: PendingNode) -> Result {
        let mut static_statements = Vec::new();
        let mut dynamic_statements = Vec::new();
        while *self.peek_token()? != TokenType::RightCurlyBrace {
            let statement = self.do_statement(node.id)?;
            match self.ast.get_node(statement).body {
                NodeBody::TypeDeclaration { .. }
                | NodeBody::StaticDeclaration { .. }
                | NodeBody::Import { .. } => static_statements.push(statement),
                _ => dynamic_statements.push(statement),
            }
        }
        let statements = {
            static_statements.append(&mut dynamic_statements);
            static_statements
        };
        self.ensure_next_token(&node, TokenType::RightCurlyBrace)?;
        Ok(self.add_node(node, NodeBody::Block { body: statements }))
    }

    fn do_statement(&mut self, parent_id: NodeID) -> Result {
        use crate::token::TokenType::*;
        let node = self.node(parent_id);
        self.pending_statement = Some(node.id);
        self.pending_expression = None;

        let node = match self.next_token(&node)? {
            Int(int, _) => Ok(self.add_node(
                node,
                NodeBody::ConstValue {
                    tp: None,
                    value: NodeValue::Int(int).into(),
                },
            )),
            Float(float, _, _) => Ok(self.add_node(
                node,
                NodeBody::ConstValue {
                    tp: None,
                    value: NodeValue::Float(float).into(),
                },
            )),
            Op(op) => self.do_operation(node, op, None),
            Name(symbol) => self.do_statement_symbol(node, &symbol),
            LeftCurlyBrace => self.do_block(node),
            EndStatement => Ok(self.add_node(node, NodeBody::Empty)),
            KeyName(keyword) => match keyword.as_ref() {
                "return" => self.do_return(node),
                "if" => self.do_if(node),
                "loop" => self.do_loop(node),
                "break" => self.do_break(node),
                "import" => self.do_import(node),
                "continue" => unimplemented!(),
                keyword => Err(self.ast.single_error(
                    &format!("Invalid keyword here '{:?}'", keyword),
                    "",
                    vec![node.id],
                )),
            },
            Comment(comment) => Ok(self.add_node(node, NodeBody::Comment(comment))),
            other => {
                Err(self
                    .ast
                    .single_error(&format!("Unknown token {:?}", other), "", vec![node.id]))
            }
        }?;
        if self.should_terminate_statement(node) {
            self.ensure_next_token_for_id(node, EndStatement, "")?;
        }
        Ok(node)
    }

    fn do_expression(&mut self, parent_id: NodeID) -> Result {
        use crate::token::TokenType::*;
        let node = self.node(parent_id);
        self.pending_expression = Some(node.id);

        let node = match self.next_token(&node)? {
            Int(int, _) => self.add_node(
                node,
                NodeBody::ConstValue {
                    tp: None,
                    value: NodeValue::Int(int).into(),
                },
            ),
            Float(float, _, _) => self.add_node(
                node,
                NodeBody::ConstValue {
                    tp: None,
                    value: NodeValue::Float(float).into(),
                },
            ),
            String(value) => self.add_node(
                node,
                NodeBody::ConstValue {
                    tp: None,
                    value: NodeValue::String(value).into(),
                },
            ),
            Op(op) => self.do_operation(node, op, None)?,
            Name(symbol) => self.do_expression_symbol(node, &symbol, None)?,
            LeftCurlyBrace => self.do_block(node)?,
            KeyName(key) => match key.as_ref() {
                "fn" => self.do_procedure(node)?,
                "true" => self.do_bool(node, true)?,
                "false" => self.do_bool(node, false)?,
                keyword => Err(self.ast.single_error(
                    &format!("Invalid keyword here '{:?}'", keyword),
                    "",
                    vec![node.id],
                ))?,
            },
            LeftBrace => match self.peek_token()? {
                RightBrace => {
                    let left = node;
                    let right = self.node(left.id);
                    self.next_token(&right)?;
                    Err(self.ast.single_error(
                        &format!("Empty parenthesis are not allowed as expressions"),
                        "Invalid expression",
                        vec![left.id, right.id],
                    ))?
                }
                _ => {
                    let expr_node = self.do_expression(node.id)?;
                    self.ensure_next_token(&node, RightBrace)?;
                    self.add_node(node, NodeBody::Expression(expr_node))
                }
            },
            other => {
                Err(self
                    .ast
                    .single_error(&format!("Unkown token {:?}", other), "", vec![node.id]))?
            }
        };

        match self.peek_token_or_none() {
            Some(TokenType::Op(op)) => {
                let lhs = node;
                let op = *op;
                let op_node = self.node(parent_id);
                // Since we parsed the lhs expression before the operation, we have to rewrite
                // lhs parent id to lie under the operation.
                self.ast.get_node_mut(lhs).parent_id = Some(op_node.id);

                self.next_token(&op_node)?;
                self.do_operation(op_node, op, Some(lhs))
            }
            _ => Ok(node),
        }
    }

    fn do_if(&mut self, node: PendingNode) -> Result {
        self.ensure_next_token(&node, TokenType::LeftBrace)?;
        let condition = self.do_expression(node.id)?;
        self.ensure_next_token(&node, TokenType::RightBrace)?;

        let body_node = self.node(node.id);
        self.ensure_next_token(&body_node, TokenType::LeftCurlyBrace)?;
        let body = self.do_block(body_node)?;
        Ok(self.add_node(node, NodeBody::If { condition, body }))
    }

    fn do_import(&mut self, node: PendingNode) -> Result {
        match self.next_token(&node)? {
            TokenType::Name(namespace) => {
                self.ensure_next_token(&node, TokenType::Dot)?;
                match self.next_token(&node)? {
                    TokenType::Name(ident) => {
                        let body_node = self.node(node.id);
                        let expr = self.add_node(
                            body_node,
                            NodeBody::Unlinked(UnlinkedNodeBody::ImportValue {
                                namespace: namespace.clone(),
                                ident: ident.clone(),
                            }),
                        );
                        Ok(self.add_node(
                            node,
                            NodeBody::Import {
                                ident,
                                namespace,
                                expr,
                            },
                        ))
                    }
                    _ => unimplemented!(),
                }
            }
            _ => unimplemented!(),
        }
    }

    fn do_bool(&mut self, node: PendingNode, value: bool) -> Result {
        Ok(self.add_node(
            node,
            NodeBody::ConstValue {
                tp: None,
                value: NodeValue::Bool(value).into(),
            },
        ))
    }

    fn do_loop(&mut self, node: PendingNode) -> Result {
        let body_node = self.node(node.id);
        self.ensure_next_token(&body_node, TokenType::LeftCurlyBrace)?;
        let body = self.do_block(body_node)?;
        Ok(self.add_node(node, NodeBody::Loop { body }))
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
        let id = node.id;
        self.do_sub_operation(node, id, op, pending_node)
    }

    fn do_sub_operation(
        &mut self,
        node: PendingNode,
        parent_id: NodeID,
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
                    NodeBody::Op { op, lhs, rhs }
                } else {
                    let node = self.node(parent_id);
                    let id = node.id;
                    let rhs = match self.next_token(&node)? {
                        TokenType::Op(op) => self.do_sub_operation(node, id, op, Some(rhs))?,
                        _ => Err(self.ast.single_error(
                            &format!("Expecting operation, found something else"),
                            "",
                            vec![node.id],
                        ))?,
                    };
                    NodeBody::Op { op, lhs, rhs }
                }
            } else {
                match op {
                    ArithmeticOP::Add | ArithmeticOP::Sub => {
                        // Prefix operation of single node
                        let rhs = self.do_expression(node.id)?;
                        NodeBody::PrefixOp { op, rhs }
                    }
                    _ => Err(self.ast.single_error(
                        &format!("Can only use prefix operations for addition and subtraction"),
                        "",
                        vec![node.id],
                    ))?,
                }
            }
        };
        Ok(self.add_node(node, body))
    }

    fn do_type(&mut self, parent: &PendingNode) -> Result<(NodeID, &PartialType)> {
        use PartialType::*;
        use TokenType::*;

        let node = self.node(parent.id);
        let token = self.next_token(&node)?;
        let (tp, parts) = match token {
            Name(ident) => match ident.as_ref() {
                "int" => (Complete(NodeType::Int), vec![]),
                "float" => (Complete(NodeType::Float), vec![]),
                "string" => (Complete(NodeType::String), vec![]),
                "bool" => (Complete(NodeType::Bool), vec![]),
                "Fn" => {
                    self.ensure_next_token(&node, LeftBrace)?;

                    let mut complete = true;
                    let mut parts = Vec::new();
                    let args = match self.peek_token()? {
                        RightBrace => {
                            self.next_token(&node)?;
                            vec![]
                        }
                        _ => {
                            let mut args = Vec::new();
                            loop {
                                let (arg_id, tp_ref) = self.do_type(&node)?;
                                if !tp_ref.is_complete() {
                                    complete = false;
                                }
                                parts.push(arg_id);
                                args.push(tp_ref.tp().clone());
                                match self.next_token(&node)? {
                                    ListSeparator => (),
                                    RightBrace => break args,
                                    _ => Err(self.ast.single_error(
                                        &format!("Invalid token found in place of ')' or ','"),
                                        "",
                                        vec![node.id],
                                    ))?,
                                }
                            }
                        }
                    };
                    let ret = if self.peek_token()? == &ReturnTypes {
                        self.next_token(&node)?;
                        let (ret_id, tp_ref) = self.do_type(&node)?;
                        if !tp_ref.is_complete() {
                            complete = false;
                        }
                        parts.push(ret_id);
                        tp_ref.tp()
                    } else {
                        let tp_ref = Complete(NodeType::Void);
                        let ret = self.node(node.id);
                        let ret_id = self.add_node(
                            ret,
                            NodeBody::PartialType {
                                parts: vec![],
                                tp: tp_ref,
                            },
                        );
                        let tp_ref = match &self.ast.get_node(ret_id).body {
                            NodeBody::PartialType { tp, .. } => tp,
                            _ => unreachable!(),
                        };
                        parts.push(ret_id);
                        tp_ref.tp()
                    };
                    let tp = NodeType::Fn {
                        args,
                        returns: Box::new(ret.clone()),
                    };
                    (PartialType::new(complete, tp), parts)
                }
                _ => (PartialType::Uncomplete(NodeType::Unknown { ident }), vec![]),
            },
            _ => Err(self.ast.single_error(
                &format!("Invalid token found when expecting type"),
                &format!(
                    "Add a type identifier like for example {} or {}",
                    "string".red(),
                    "int".red()
                ),
                vec![node.id],
            ))?,
        };
        let node_id = self.add_node(node, NodeBody::PartialType { parts, tp });
        let tp_ref = match &self.ast.get_node(node_id).body {
            NodeBody::PartialType { tp, .. } => tp,
            _ => unreachable!(),
        };
        Ok((node_id, tp_ref))
    }

    fn default_value(&self, tp: &NodeType) -> (PartialNodeValue, bool) {
        let mut linked = true;
        let value = match tp {
            NodeType::Int => NodeValue::Int(0).into(),
            NodeType::Bool => NodeValue::Bool(false).into(),
            NodeType::Float => NodeValue::Float(0.0).into(),
            NodeType::String => NodeValue::String("".into()).into(),
            NodeType::Struct { fields } => NodeValue::Struct(
                fields
                    .iter()
                    .map(|(name, tp)| {
                        let (tp, is_linked) = self.default_value(tp);
                        if !is_linked {
                            linked = false;
                        }
                        (name.clone(), tp)
                    })
                    .collect(),
            )
            .into(),
            NodeType::Unknown { ident } => {
                linked = false;
                PartialNodeValue::Unlinked(ident.clone())
            }
            _ => unimplemented!(),
        };
        (value, linked)
    }

    fn do_type_definition(&mut self, node: PendingNode, ident: &str) -> Result {
        self.ensure_next_token(&node, TokenType::LeftCurlyBrace)?;

        let mut parts = Vec::new();
        let mut fields = Vec::new();
        let mut complete = true;
        loop {
            match self.next_token(&node)? {
                TokenType::Name(key) => {
                    self.ensure_next_token(&node, TokenType::TypeDeclaration)?;
                    let (id, tp_ref) = self.do_type(&node)?;
                    if !tp_ref.is_complete() {
                        complete = false;
                    }
                    parts.push(id);
                    fields.push((key, tp_ref.tp().clone()));
                }
                TokenType::RightCurlyBrace => break,
                _ => Err(self.ast.single_error(
                    &format!("Invalid token found in place of '}}' or field"),
                    "",
                    vec![node.id],
                ))?,
            }
            if self.peek_token()? == &TokenType::ListSeparator {
                self.next_token(&node)?;
            }
        }

        let self_tp = NodeType::Struct { fields };
        let tp_ref = PartialType::new(
            complete,
            NodeType::NewType {
                tp: Box::new(NodeType::Type {
                    ident: ident.into(),
                    content: Box::new(self_tp.clone()),
                }),
            },
        );
        let tp = {
            let tp = self.node(node.id);
            self.add_node(tp, NodeBody::PartialType { parts, tp: tp_ref })
        };
        let mut default_value = None;
        let constructor = {
            let node = self.node(node.id);

            let body = {
                let node = self.node(node.id);

                let ret_id = {
                    let node = self.node(node.id);

                    let value_id = {
                        let node = self.node(node.id);
                        let (value, is_linked) = self.default_value(&self_tp);
                        if is_linked {
                            default_value = Some(value.clone());
                            let tp = {
                                let node = self.node(node.id);
                                self.add_node(node, NodeBody::TypeReference { tp })
                            };
                            let value = NodeBody::ConstValue {
                                tp: Some(tp),
                                value,
                            };
                            self.add_node(node, value)
                        } else {
                            let value = UnlinkedNodeBody::Value {
                                tp: Some(tp),
                                value,
                            };
                            self.add_uncomplete_node(node, value)
                        }
                    };

                    let ret = NodeBody::Unlinked(UnlinkedNodeBody::Return {
                        expr: Some(value_id),
                        automatic: true,
                    });
                    self.add_node(node, ret)
                };

                let body = NodeBody::Block { body: vec![ret_id] };
                self.add_node(node, body)
            };

            let tp = {
                let node = self.node(node.id);
                self.add_node(node, NodeBody::TypeReference { tp })
            };
            let constructor = NodeBody::ProcedureDeclaration(NBProcedureDeclaration {
                args: Vec::new(),
                returns: Some(tp),
                body,
            });
            ProcedureDeclarationNode::try_new(self.add_node(node, constructor), &self.ast).unwrap()
        };

        let body = NodeBody::TypeDeclaration {
            ident: ident.into(),
            tp,
            constructor,
            default_value,
        };
        let node_id = self.add_node(node, body);
        Ok(node_id)
    }

    fn do_procedure(&mut self, node: PendingNode) -> Result {
        self.ensure_next_token(&node, TokenType::LeftBrace)?;
        let mut args = Vec::new();

        while let Some(_) = self.peek_token_not(&TokenType::RightBrace)? {
            let arg_node = self.node(node.id);
            match self.next_token(&arg_node)? {
                TokenType::Name(ident) => {
                    args.push(arg_node.id);
                    let (id, _) = match self.peek_token() {
                        Ok(TokenType::TypeDeclaration) => {
                            self.next_token(&arg_node)?;
                            match self.do_type(&arg_node) {
                                Ok(v) => v,
                                Err(_) => {
                                    return Err(self.ast.single_error(
                                        "Reached en of file.",
                                        &format!("Expecting a type parameter like {}", "int".red()),
                                        self.ast.nodes_after(node.id),
                                    ))?;
                                }
                            }
                        }
                        Ok(_) => {
                            let token = self.next_token(&arg_node)?;
                            Err(self.ast.single_error(
                                &format!(
                                    "Expecting type declaration {}, found {}",
                                    TokenType::TypeDeclaration,
                                    token
                                ),
                                &format!("Add {} before {}", TokenType::TypeDeclaration, token),
                                self.ast.nodes_after(node.id),
                            ))?
                        }
                        Err(_) => Err(self.ast.single_error(
                            &format!("Reached en of file."),
                            &format!("Expecting type declaration {}", ":".red()),
                            self.ast.nodes_after(node.id),
                        ))?,
                    };
                    self.add_node(
                        arg_node,
                        NodeBody::VariableDeclaration {
                            ident,
                            tp: Some(id),
                            expr: None,
                        },
                    );
                }
                _ => Err(self.ast.single_error(
                    &format!("Invalid token found when expecting argument"),
                    "",
                    vec![arg_node.id],
                ))?,
            }

            match self.peek_token() {
                Ok(&TokenType::ListSeparator) => {
                    self.next_token(&node)?;
                }
                Ok(_) => break,
                Err(_) =>
                    Err(self.ast.single_error(
                        &format!("Reached en of file."),
                        &format!(
                            "Either add another argument, separated with {} or close the expression with {}",
                            ",".red(),
                            ")".red()
                        ),
                        self.ast.nodes_after(node.id),
                    ))?
            }
        }

        if self.peek_token().is_err() {
            Err(self.ast.single_error(
                &format!("Reached en of file."),
                &format!("Add {}", ")".red()),
                self.ast.nodes_after(node.id),
            ))?
        }
        self.ensure_next_token(&node, TokenType::RightBrace)?;
        let returns = match self.peek_token() {
            Ok(TokenType::ReturnTypes) => {
                self.next_token(&node)?;
                let (id, _) = self.do_type(&node)?;
                Some(id)
            }
            _ => None,
        };

        let mut block_node = self.node(node.id);
        self.ensure_next_token(&mut block_node, TokenType::LeftCurlyBrace)?;
        let body = self.do_block(block_node)?;

        let children = self.ast.get_node(body).body.children();

        let has_return = {
            // TODO: Optimize. We are lazy and just copy the entire array instead of implementing a double ended iterator.
            let children = children.collect::<Vec<&NodeID>>();
            let mut has_return = false;
            for last_id in children.into_iter().rev() {
                let last = self.ast.get_node(*last_id);
                match last.body {
                    // Ignore comments when searching for last statement
                    NodeBody::Comment(..) => continue,
                    NodeBody::Unlinked(UnlinkedNodeBody::Return { .. }) => {
                        has_return = true;
                        break;
                    }
                    _ => break,
                }
            }
            has_return
        };
        if !has_return {
            let ret_node = self.node(body);
            let ret_node = self.add_uncomplete_node(
                ret_node,
                UnlinkedNodeBody::Return {
                    expr: None,
                    automatic: true,
                },
            );
            match &mut self.ast.get_node_mut(body).body {
                NodeBody::Block { body: children } => {
                    children.push(ret_node);
                }
                _ => unreachable!(),
            }
        }
        Ok(self.add_node(
            node,
            NodeBody::ProcedureDeclaration(NBProcedureDeclaration {
                args,
                returns,
                body,
            }),
        ))
    }

    fn do_statement_symbol(&mut self, node: PendingNode, ident: &str) -> Result {
        use crate::token::TokenType::*;

        match self.peek_token()? {
            LeftBrace => self.do_function_call(node, ident),
            TypeDeclaration => {
                self.next_token(&node)?;
                let next = self.peek_token()?;
                if let Assignment = next {
                    // TypeDeclaration + Assignment = VariableDeclaration
                    return self.do_variable_declaration(node, ident, None);
                }
                let (tp, _) = self.do_type(&node)?;
                match self.peek_token()? {
                    ConstDeclaration | VariableDeclaration => {
                        self.do_variable_or_assignment(node, ident, Some(tp))
                    }
                    EndStatement => {
                        let node = self.add_node(
                            node,
                            NodeBody::VariableDeclaration {
                                ident: ident.into(),
                                tp: Some(tp),
                                expr: None,
                            },
                        );
                        Ok(node)
                    }
                    token => {
                        let token = token.clone();
                        Err(self.ast.single_error(&format!(
                            "Unexpected token {:?} after parsing variable type '{:?}' for '{:?}'. '::', ':=', or ';' expected",
                            token, tp, ident
                        ), "", vec![node.id]))
                    }
                }
            }
            _ => self.do_variable_or_assignment(node, ident, None),
        }
    }

    fn find_traverse_path(&mut self, node: &PendingNode) -> Result<Vec<String>> {
        use crate::token::TokenType::*;
        let mut path = Vec::new();

        loop {
            match self.peek_token()? {
                Dot => {
                    self.next_token(node)?;
                    match self.next_token(node)? {
                        Name(field) => {
                            path.push(field);
                        }
                        _ => {
                            return Err(self.ast.single_error(
                                "A name is require after a '.' operator",
                                "",
                                vec![node.id],
                            ));
                        }
                    }
                }
                _ => break,
            }
        }
        Ok(path)
    }

    fn do_variable_or_assignment(
        &mut self,
        node: PendingNode,
        ident: &str,
        tp: Option<NodeID>,
    ) -> Result {
        use crate::token::TokenType::*;

        let token = self.peek_token()?;
        match token {
            ReturnTypes => {
                self.next_token(&node)?;
                self.ensure_next_token(&node, TokenType::KeyName("type".into()))?;
                self.do_type_definition(node, ident)
            }
            ConstDeclaration => {
                self.next_token(&node)?;
                let expr = self.do_expression(node.id)?;
                let node = match self.ast.get_node(expr).body {
                    NodeBody::ProcedureDeclaration(NBProcedureDeclaration { .. })
                    | NodeBody::ConstValue { .. } => self.add_node(
                        node,
                        NodeBody::StaticDeclaration {
                            ident: ident.into(),
                            tp,
                            expr,
                        },
                    ),
                    _ => self.add_node(
                        node,
                        NodeBody::ConstDeclaration {
                            ident: ident.into(),
                            tp,
                            expr,
                        },
                    ),
                };
                Ok(node)
            }
            VariableDeclaration => self.do_variable_declaration(node, ident, tp),
            Assignment => {
                self.next_token(&node)?;
                let expr = self.do_expression(node.id)?;
                let node = self.add_uncomplete_node(
                    node,
                    UnlinkedNodeBody::VariableAssignment {
                        ident: ident.into(),
                        path: None,
                        expr,
                    },
                );
                Ok(node)
            }
            Dot => {
                let path = self.find_traverse_path(&node)?;
                match self.peek_token()? {
                    Assignment => {
                        self.next_token(&node)?;
                        let expr = self.do_expression(node.id)?;
                        let node = self.add_uncomplete_node(
                            node,
                            UnlinkedNodeBody::VariableAssignment {
                                ident: ident.into(),
                                path: Some(path),
                                expr,
                            },
                        );
                        Ok(node)
                    }
                    _ => unimplemented!(),
                }
            }
            _ => {
                let details = &format!(
                    "Unexpected token: {:?} when parsing identifier with name {:?}",
                    token, ident
                );
                Err(self.ast.single_error(details, "", vec![node.id]))
            }
        }
    }

    fn do_variable_declaration(
        &mut self,
        node: PendingNode,
        ident: &str,
        tp: Option<NodeID>,
    ) -> Result {
        self.next_token(&node)?;
        let expr = self.do_expression(node.id)?;
        let node = self.add_node(
            node,
            NodeBody::VariableDeclaration {
                ident: ident.into(),
                tp,
                expr: Some(expr),
            },
        );
        Ok(node)
    }

    fn do_expression_symbol(
        &mut self,
        node: PendingNode,
        ident: &str,
        path: Option<Vec<String>>,
    ) -> Result {
        use crate::token::TokenType::*;

        let token = self.peek_token()?;
        match token {
            LeftBrace => self.do_function_call(node, ident),
            Op(_) | RightBrace | EndStatement | ListSeparator => Ok(self.add_uncomplete_node(
                node,
                UnlinkedNodeBody::VariableValue {
                    ident: ident.into(),
                    path,
                },
            )),
            Dot => {
                let path = self.find_traverse_path(&node)?;
                self.do_expression_symbol(node, ident, Some(path))
            }
            _ => {
                let details = &format!(
                    "Unkown token: {:?} when parsing symbol with name {:?}",
                    token, ident
                );
                Err(self.ast.single_error(details, "", vec![node.id]))
            }
        }
    }

    fn do_function_call(&mut self, node: PendingNode, ident: &str) -> Result {
        use crate::token::TokenType::*;

        self.ensure_next_token(&node, LeftBrace)?;
        let mut args = Vec::new();
        while self.peek_token()? != &RightBrace {
            let arg = self.do_expression(node.id)?;
            args.push(arg);
            if self.peek_token()? == &ListSeparator {
                self.next_token(&node)?;
            }
        }
        self.ensure_next_token(&node, RightBrace)?;
        Ok(self.add_uncomplete_node(
            node,
            UnlinkedNodeBody::Call {
                ident: ident.into(),
                args,
            },
        ))
    }

    fn do_return(&mut self, node: PendingNode) -> Result {
        let expr = if self.peek_token()? != &EndStatement {
            let ret_id = self.do_expression(node.id)?;
            Some(ret_id)
        } else {
            None
        };
        Ok(self.add_uncomplete_node(
            node,
            UnlinkedNodeBody::Return {
                expr,
                automatic: false,
            },
        ))
    }
}
