use super::{Ast, NodeID, NodeValue, Result};
use crate::ast;
use crate::ast::ast::{AstID, PartialNodeValue, PartialType, ProcedureDeclarationNode};
use crate::ast::nodebody::{NBProcedureDeclaration, NodeBody, UnlinkedNodeBody};
use crate::ast::NodeType;
use crate::token::KeyName;
use crate::token::TokenType::EndStatement;
use crate::token::{ArithmeticOP, Token, TokenType};
use ast::Err;
use colored::Colorize;
use std::iter::Peekable;
use std::result;

pub fn ast_from_tokens<I>(
    file_name: String,
    ast_id: AstID,
    iter: I,
    size_hint: usize,
) -> result::Result<Ast, (ast::Ast, ast::Err)>
where
    I: Iterator<Item = Token>,
{
    let mut parser = Parser::new(file_name, ast_id, iter, size_hint);
    match parser.parse() {
        Ok(()) => Ok(parser.ast),
        Err(err) => Err((parser.ast, err)),
    }
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

struct PendingToken<'a> {
    ast: &'a Ast,
    node: NodeID,
    got: Option<&'a TokenType>,
}

fn expected_to_message(expected: &[&TokenType]) -> String {
    match expected {
        &[] => "something".to_string(),
        &[one] => format!("{}", one),
        many => [
            "one of { ",
            &many
                .iter()
                .map(|t| format!("{}", t))
                .collect::<Vec<String>>()
                .join(", "),
            " }",
        ]
        .join(""),
    }
}

fn missing_token_error(ast: &Ast, node: NodeID, expected: &[&TokenType]) -> ast::Err {
    let expected_message = expected_to_message(expected);
    Err::single(
        &format!("Reached en of file while looking for {}", expected_message),
        &format!("Consider adding {}", expected_message.to_string().red()),
        ast.nodes_after(node),
    )
}

fn invalid_keyword_error(ast: &Ast, node: NodeID, got: KeyName, expected: &[KeyName]) -> ast::Err {
    let expected_message = match expected {
        &[] => unreachable!(),
        &[one] => format!("{}", one),
        many => [
            "one of { ",
            &many
                .iter()
                .map(|t| format!("{}", t))
                .collect::<Vec<String>>()
                .join(", "),
            " }",
        ]
        .join(""),
    };

    Err::single(
        &format!("Found {} while expecting {}", got, expected_message),
        &format!(
            "Consider replacing with {}",
            expected_message.to_string().red()
        ),
        ast.nodes_after(node),
    )
}

fn wrong_token_error(
    ast: &Ast,
    node: NodeID,
    got: &TokenType,
    expected: &[&TokenType],
) -> ast::Err {
    let expected_message = expected_to_message(expected);
    Err::single(
        &format!("Found {} while expecting {}", got, expected_message),
        &format!(
            "Consider replacing with {}",
            expected_message.to_string().red()
        ),
        ast.nodes_after(node),
    )
}

impl<'a> PendingToken<'a> {
    fn new(ast: &'a Ast, node: NodeID, got: Option<&'a TokenType>) -> Self {
        PendingToken { ast, node, got }
    }

    fn missing_token_error(self, expected: &TokenType) -> ast::Err {
        missing_token_error(self.ast, self.node, &[expected])
    }

    fn wrong_token_error(self, expected: &TokenType) -> ast::Err {
        wrong_token_error(self.ast, self.node, self.got.unwrap(), &[expected])
    }

    fn expect(self, other: &TokenType) -> Result<&'a TokenType> {
        match self.got {
            Some(tp) => match tp.is(other) {
                true => Ok(tp),
                false => Err(self.wrong_token_error(other)),
            },
            None => Err(self.missing_token_error(other)),
        }
    }

    fn expect_exact(self, other: &TokenType) -> Result<&'a TokenType> {
        match self.got {
            Some(tp) => match tp == other {
                true => Ok(tp),
                false => Err(self.wrong_token_error(other)),
            },
            None => Err(self.missing_token_error(other)),
        }
    }

    fn any(self, recommendation: Option<&TokenType>) -> Result<&'a TokenType> {
        match self.got {
            Some(tp) => Ok(tp),
            None => {
                let expected = if let Some(tp) = recommendation {
                    vec![tp]
                } else {
                    vec![]
                };
                Err(missing_token_error(self.ast, self.node, &expected))
            }
        }
    }

    fn is(self, other: &TokenType) -> Result<bool> {
        match self.got {
            Some(tp) => Ok(tp.is(other)),
            None => Err(self.missing_token_error(other)),
        }
    }

    fn not(self, other: &TokenType) -> Result<bool> {
        self.is(other).map(|v| !v)
    }
}

impl<I> Parser<I>
where
    I: Iterator<Item = Token>,
{
    fn new(file_name: String, ast_id: AstID, iter: I, size_hint: usize) -> Self {
        let ast = Ast::new(file_name, ast_id, size_hint);

        Self {
            ast,
            iter: iter.peekable(),
            pending_expression: None,
            pending_statement: None,
        }
    }

    fn parse(&mut self) -> Result<()> {
        let node = self.any_node(None);
        let mut static_statements = Vec::new();
        let mut import_statements = Vec::new();
        let mut dynamic_statements = Vec::new();
        while self.peek_token().any(None).is_ok() {
            let statement = self.do_statement(node.id)?;
            match self.ast.get_node(statement).body {
                NodeBody::TypeDeclaration { .. } | NodeBody::StaticDeclaration { .. } => {
                    static_statements.push(statement)
                }
                NodeBody::Import { .. } => import_statements.push(statement),
                _ => dynamic_statements.push(statement),
            }
        }
        self.add_node(
            node,
            NodeBody::Block {
                static_body: static_statements,
                import_body: import_statements,
                dynamic_body: dynamic_statements,
            },
        );
        Ok(())
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

    fn eat_token(&mut self, node: &PendingNode) {
        self.next_token(node).any(None).unwrap();
    }

    fn peek_token(&mut self) -> PendingToken {
        let tp = self.iter.peek().map(|t| &t.tp);
        let node = self
            .pending_expression
            .unwrap_or_else(|| self.pending_statement.unwrap_or_else(|| self.ast.root()));
        PendingToken::new(&self.ast, node, tp)
    }

    fn next_token(&mut self, node: &PendingNode) -> PendingToken {
        let tp = match self.iter.next() {
            Some(token) => {
                self.ast.get_node_mut(node.id).tokens.push(token);
                let tp = &self.ast.get_node(node.id).tokens.last().unwrap().tp;
                Some(tp)
            }
            None => None,
        };
        PendingToken::new(&self.ast, node.id, tp)
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
        let mut import_statements = Vec::new();
        let mut dynamic_statements = Vec::new();
        while self.peek_token().not(&TokenType::RightCurlyBrace)? {
            let statement = self.do_statement(node.id)?;
            match self.ast.get_node(statement).body {
                NodeBody::TypeDeclaration { .. } | NodeBody::StaticDeclaration { .. } => {
                    static_statements.push(statement)
                }
                NodeBody::Import { .. } => import_statements.push(statement),
                _ => dynamic_statements.push(statement),
            }
        }
        self.next_token(&node).expect(&TokenType::RightCurlyBrace)?;
        Ok(self.add_node(
            node,
            NodeBody::Block {
                static_body: static_statements,
                import_body: import_statements,
                dynamic_body: dynamic_statements,
            },
        ))
    }

    fn do_statement(&mut self, parent_id: NodeID) -> Result {
        use crate::token::KeyName::*;
        use crate::token::TokenType::*;
        let node = self.node(parent_id);
        self.pending_statement = Some(node.id);
        self.pending_expression = None;

        let node = match self.next_token(&node).any(None)? {
            Int(int, _) => {
                let int = *int;
                Ok(self.add_node(
                    node,
                    NodeBody::ConstValue {
                        tp: None,
                        value: NodeValue::Int(int).into(),
                    },
                ))
            }
            Float(float, _, _) => {
                let float = *float;
                Ok(self.add_node(
                    node,
                    NodeBody::ConstValue {
                        tp: None,
                        value: NodeValue::Float(float).into(),
                    },
                ))
            }
            Op(op) => {
                let op = *op;
                self.do_operation(node, op, None)
            }
            Name(symbol) => {
                let symbol = symbol.to_string();
                self.do_statement_symbol(node, symbol)
            }
            LeftCurlyBrace => self.do_block(node),
            EndStatement => Ok(self.add_node(node, NodeBody::Empty)),
            KeyName(Return) => self.do_return(node),
            KeyName(If) => self.do_if(node),
            KeyName(Loop) => self.do_loop(node),
            KeyName(Break) => self.do_break(node),
            KeyName(Import) => self.do_import(node),
            KeyName(Continue) => unimplemented!(),
            KeyName(keyword) => {
                let keyword = *keyword;
                Err(invalid_keyword_error(
                    &self.ast,
                    node.id,
                    keyword,
                    &[Return, If, Loop, Break, Import, Continue],
                ))?
            }
            Comment(comment) => {
                let comment = comment.clone();
                Ok(self.add_node(node, NodeBody::Comment(comment)))
            }
            token => {
                let token = token.clone();
                Err(wrong_token_error(
                    &self.ast,
                    node.id,
                    &token,
                    &[
                        &Int(1, 0),
                        &Float(1.0, 0, 0),
                        &Op(ArithmeticOP::Add),
                        &Name("variable".to_string()),
                        &LeftCurlyBrace,
                        &EndStatement,
                        &KeyName(If),
                        &Comment("comment".to_string()),
                    ],
                ))
            }
        }?;
        if self.should_terminate_statement(node) {
            let node = PendingNode { id: node };
            self.next_token(&node).expect(&EndStatement)?;
        }
        Ok(node)
    }

    fn do_expression(&mut self, parent_id: NodeID) -> Result {
        use crate::token::KeyName::*;
        use crate::token::TokenType::*;
        let node = self.node(parent_id);
        self.pending_expression = Some(node.id);

        let node = match self.next_token(&node).any(Some(&Int(1, 0)))? {
            Int(int, _) => {
                let int = *int;
                self.add_node(
                    node,
                    NodeBody::ConstValue {
                        tp: None,
                        value: NodeValue::Int(int).into(),
                    },
                )
            }
            Float(float, _, _) => {
                let float = *float;
                self.add_node(
                    node,
                    NodeBody::ConstValue {
                        tp: None,
                        value: NodeValue::Float(float).into(),
                    },
                )
            }
            String(value) => {
                let value = value.clone();
                self.add_node(
                    node,
                    NodeBody::ConstValue {
                        tp: None,
                        value: NodeValue::String(value).into(),
                    },
                )
            }
            Op(op) => {
                let op = *op;
                self.do_operation(node, op, None)?
            }
            Name(symbol) => {
                let symbol = symbol.to_string();
                self.do_expression_symbol(node, symbol, None)?
            }
            LeftCurlyBrace => self.do_block(node)?,
            KeyName(Fn) => self.do_procedure(node)?,
            KeyName(True) => self.do_bool(node, true)?,
            KeyName(False) => self.do_bool(node, false)?,
            KeyName(keyword) => {
                let keyword = *keyword;
                Err(invalid_keyword_error(
                    &self.ast,
                    node.id,
                    keyword,
                    &[Fn, True, False],
                ))?
            }
            LeftBrace => {
                if self.peek_token().not(&RightBrace)? {
                    let expr_node = self.do_expression(node.id)?;
                    self.next_token(&node).expect(&RightBrace)?;
                    self.add_node(node, NodeBody::Expression(expr_node))
                } else {
                    let left = node;
                    let right = self.node(left.id);
                    self.eat_token(&right);
                    Err(Err::single(
                        &format!("Empty parenthesis are not allowed as expressions"),
                        "Invalid expression",
                        vec![left.id, right.id],
                    ))?
                }
            }
            token => {
                let token = token.clone();
                Err(wrong_token_error(
                    &self.ast,
                    node.id,
                    &token,
                    &[
                        &Int(1, 0),
                        &Float(1.0, 0, 0),
                        &String("".to_string()),
                        &Op(ArithmeticOP::Add),
                        &Name("variable".to_string()),
                        &LeftCurlyBrace,
                        &KeyName(If),
                        &LeftBrace,
                    ],
                ))?
            }
        };

        match self.peek_token().any(None) {
            Ok(TokenType::Op(op)) => {
                let lhs = node;
                let op = *op;
                let op_node = self.node(parent_id);
                // Since we parsed the lhs expression before the operation, we have to rewrite
                // lhs parent id to lie under the operation.
                self.ast.get_node_mut(lhs).parent_id = Some(op_node.id);

                self.eat_token(&op_node);
                self.do_operation(op_node, op, Some(lhs))
            }
            _ => Ok(node),
        }
    }

    fn do_if(&mut self, node: PendingNode) -> Result {
        self.next_token(&node).expect(&TokenType::LeftBrace)?;
        let condition = self.do_expression(node.id)?;
        self.next_token(&node).expect(&TokenType::RightBrace)?;

        let body_node = self.node(node.id);
        self.next_token(&body_node)
            .expect(&TokenType::LeftCurlyBrace)?;
        let body = self.do_block(body_node)?;
        Ok(self.add_node(node, NodeBody::If { condition, body }))
    }

    fn do_import(&mut self, node: PendingNode) -> Result {
        // TODO: this can be constant
        let default_module = TokenType::Name("std".to_string());

        match self.next_token(&node).expect(&default_module)? {
            TokenType::Name(module) => {
                let module = module.clone();
                let path = self.find_traverse_path(&node)?;
                let body_node = self.node(node.id);
                let expr = self.add_node(
                    body_node,
                    NodeBody::Unlinked(UnlinkedNodeBody::ImportValue {
                        module: module.clone(),
                        path: path.clone(),
                    }),
                );
                Ok(self.add_node(node, NodeBody::Import { path, module, expr }))
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
        self.next_token(&body_node)
            .expect(&TokenType::LeftCurlyBrace)?;
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
                let next_token = self.peek_token().any(None)?;

                let lhs_precedence = Self::op_precedence(op);
                let rhs_precedence = Self::token_precedence(next_token);
                if lhs_precedence >= rhs_precedence {
                    NodeBody::Op { op, lhs, rhs }
                } else {
                    let node = self.node(parent_id);
                    let id = node.id;
                    let rhs = match self
                        .next_token(&node)
                        .expect(&TokenType::Op(ArithmeticOP::Add))?
                    {
                        TokenType::Op(op) => {
                            let op = *op;
                            self.do_sub_operation(node, id, op, Some(rhs))?
                        }
                        _ => unreachable!(),
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
                    _ => Err(Err::single(
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
        // TODO: this can be constant
        let default_type = TokenType::Name("int".to_string());

        use PartialType::*;
        use TokenType::*;

        let node = self.node(parent.id);
        let (tp, parts) = match self.next_token(&node).expect(&default_type)? {
            Name(ident) => match ident.as_ref() {
                "int" => (Complete(NodeType::Int), vec![]),
                "float" => (Complete(NodeType::Float), vec![]),
                "string" => (Complete(NodeType::String), vec![]),
                "bool" => (Complete(NodeType::Bool), vec![]),
                "Fn" => {
                    self.next_token(&node).expect(&LeftBrace)?;

                    let mut complete = true;
                    let mut parts = Vec::new();
                    let args = match self.peek_token().any(Some(&RightBrace))? {
                        RightBrace => {
                            self.eat_token(&node);
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
                                match self.next_token(&node).any(Some(&RightBrace))? {
                                    ListSeparator => (),
                                    RightBrace => break args,
                                    token => {
                                        let token = token.clone();
                                        Err(wrong_token_error(
                                            &self.ast,
                                            node.id,
                                            &token,
                                            &[&ListSeparator, &RightBrace],
                                        ))?
                                    }
                                }
                            }
                        }
                    };
                    let ret = if self.peek_token().is(&ReturnTypes)? {
                        self.eat_token(&node);
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
                _ => (
                    PartialType::Uncomplete(NodeType::Unknown {
                        ident: ident.clone(),
                    }),
                    vec![],
                ),
            },
            _ => unreachable!(),
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
            NodeType::String => NodeValue::String("".to_string()).into(),
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

    fn do_type_definition(&mut self, node: PendingNode, ident: String) -> Result {
        self.next_token(&node).expect(&TokenType::LeftCurlyBrace)?;

        let mut parts = Vec::new();
        let mut fields = Vec::new();
        let mut complete = true;
        loop {
            match self
                .next_token(&node)
                .any(Some(&TokenType::RightCurlyBrace))?
            {
                TokenType::Name(key) => {
                    let key = key.clone();
                    self.next_token(&node).expect(&TokenType::TypeDeclaration)?;
                    let (id, tp_ref) = self.do_type(&node)?;
                    if !tp_ref.is_complete() {
                        complete = false;
                    }
                    parts.push(id);
                    fields.push((key, tp_ref.tp().clone()));
                }
                TokenType::RightCurlyBrace => break,
                token => {
                    let token = token.clone();
                    Err(wrong_token_error(
                        &self.ast,
                        node.id,
                        &token,
                        &[
                            &TokenType::Name("field".to_string()),
                            &TokenType::RightCurlyBrace,
                        ],
                    ))?
                }
            }
            if self.peek_token().is(&TokenType::ListSeparator)? {
                self.eat_token(&node);
            }
        }

        let self_tp = NodeType::Struct { fields };
        let tp_ref = PartialType::new(
            complete,
            NodeType::NewType {
                tp: Box::new(NodeType::Type {
                    ident: ident.clone(),
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

                let body = NodeBody::Block {
                    static_body: vec![],
                    import_body: vec![],
                    dynamic_body: vec![ret_id],
                };
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
            ident,
            tp,
            constructor,
            default_value,
        };
        let node_id = self.add_node(node, body);
        Ok(node_id)
    }

    fn do_procedure(&mut self, node: PendingNode) -> Result {
        self.next_token(&node).expect(&TokenType::LeftBrace)?;
        let mut args = Vec::new();

        while self.peek_token().not(&TokenType::RightBrace)? {
            let arg_node = self.node(node.id);
            args.push(arg_node.id);
            match self
                .next_token(&arg_node)
                .expect(&TokenType::Name(String::new()))?
            {
                TokenType::Name(ident) => {
                    let ident = ident.clone();
                    let tp = match self
                        .next_token(&arg_node)
                        .expect(&TokenType::TypeDeclaration)?
                    {
                        TokenType::TypeDeclaration => Some(self.do_type(&arg_node)?.0),
                        _ => unreachable!(),
                    };
                    let expr = None;
                    let body = NodeBody::VariableDeclaration { ident, tp, expr };
                    self.add_node(arg_node, body);
                }
                _ => unreachable!(),
            }

            if self.peek_token().not(&TokenType::ListSeparator)? {
                break;
            }
            self.eat_token(&node);
        }

        self.next_token(&node).expect(&TokenType::RightBrace)?;

        // This is a NoOp for all cases except when we hit the end of the file.
        // The { is a better hint than a type declaration
        self.peek_token().is(&TokenType::LeftCurlyBrace)?;

        let returns = match self.peek_token().is(&TokenType::ReturnTypes)? {
            true => {
                self.eat_token(&node);
                Some(self.do_type(&node)?.0)
            }
            false => None,
        };

        let block_node = self.node(node.id);
        self.next_token(&block_node)
            .expect(&TokenType::LeftCurlyBrace)?;
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
                NodeBody::Block { dynamic_body, .. } => {
                    dynamic_body.push(ret_node);
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

    fn do_statement_symbol(&mut self, node: PendingNode, ident: String) -> Result {
        use crate::token::TokenType::*;

        match self.peek_token().any(Some(&ConstDeclaration))? {
            LeftBrace => self.do_function_call(node, ident),
            TypeDeclaration => {
                self.eat_token(&node);
                if self.peek_token().is(&Assignment)? {
                    // TypeDeclaration + Assignment = VariableDeclaration
                    return self.do_variable_declaration(node, ident, None);
                }
                let (tp, _) = self.do_type(&node)?;
                match self.peek_token().any(Some(&EndStatement))? {
                    ConstDeclaration | VariableDeclaration => {
                        self.do_variable_or_assignment(node, ident, Some(tp))
                    }
                    EndStatement => {
                        let node = self.add_node(
                            node,
                            NodeBody::VariableDeclaration {
                                ident,
                                tp: Some(tp),
                                expr: None,
                            },
                        );
                        Ok(node)
                    }
                    token => {
                        let token = token.clone();
                        Err(wrong_token_error(
                            &self.ast,
                            node.id,
                            &token,
                            &[&ConstDeclaration, &VariableDeclaration, &EndStatement],
                        ))
                    }
                }
            }
            _ => self.do_variable_or_assignment(node, ident, None),
        }
    }

    fn find_traverse_path(&mut self, node: &PendingNode) -> Result<Vec<String>> {
        use crate::token::TokenType::{Dot, Name};
        // TODO: this can be constant
        let default_name = Name("part".to_string());

        let mut path = Vec::new();
        loop {
            if self.peek_token().is(&Dot)? {
                self.eat_token(node);
                match self.next_token(node).expect(&default_name)? {
                    Name(field) => {
                        path.push(field.clone());
                    }
                    _ => unreachable!(),
                }
            } else {
                break;
            }
        }
        Ok(path)
    }

    fn do_variable_or_assignment(
        &mut self,
        node: PendingNode,
        ident: String,
        tp: Option<NodeID>,
    ) -> Result {
        use crate::token::KeyName::Type;
        use crate::token::TokenType::*;

        match self.peek_token().any(Some(&ConstDeclaration))? {
            ReturnTypes => {
                self.eat_token(&node);
                self.next_token(&node).expect_exact(&KeyName(Type))?;
                self.do_type_definition(node, ident)
            }
            ConstDeclaration => {
                self.eat_token(&node);
                let expr = self.do_expression(node.id)?;
                let node = match self.ast.get_node(expr).body {
                    NodeBody::ProcedureDeclaration(NBProcedureDeclaration { .. })
                    | NodeBody::ConstValue { .. } => {
                        self.add_node(node, NodeBody::StaticDeclaration { ident, tp, expr })
                    }
                    _ => self.add_node(node, NodeBody::ConstDeclaration { ident, tp, expr }),
                };
                Ok(node)
            }
            VariableDeclaration => self.do_variable_declaration(node, ident, tp),
            Assignment => {
                self.eat_token(&node);
                let expr = self.do_expression(node.id)?;
                let path = None;
                let node = self.add_uncomplete_node(
                    node,
                    UnlinkedNodeBody::VariableAssignment { ident, path, expr },
                );
                Ok(node)
            }
            Dot => {
                let path = Some(self.find_traverse_path(&node)?);
                match self.peek_token().expect(&Assignment)? {
                    Assignment => {
                        self.eat_token(&node);
                        let expr = self.do_expression(node.id)?;
                        let node = self.add_uncomplete_node(
                            node,
                            UnlinkedNodeBody::VariableAssignment { ident, path, expr },
                        );
                        Ok(node)
                    }
                    _ => unimplemented!(),
                }
            }
            token => {
                let token = token.clone();
                Err(wrong_token_error(
                    &self.ast,
                    node.id,
                    &token,
                    &[
                        &ReturnTypes,
                        &ConstDeclaration,
                        &VariableDeclaration,
                        &Assignment,
                        &Dot,
                    ],
                ))
            }
        }
    }

    fn do_variable_declaration(
        &mut self,
        node: PendingNode,
        ident: String,
        tp: Option<NodeID>,
    ) -> Result {
        self.eat_token(&node);
        let expr = Some(self.do_expression(node.id)?);
        let node = self.add_node(node, NodeBody::VariableDeclaration { ident, tp, expr });
        Ok(node)
    }

    fn do_expression_symbol(
        &mut self,
        node: PendingNode,
        ident: String,
        path: Option<Vec<String>>,
    ) -> Result {
        use crate::token::TokenType::*;

        match self.peek_token().any(Some(&EndStatement))? {
            LeftBrace => self.do_function_call(node, ident),
            Op(_) | RightBrace | EndStatement | ListSeparator => {
                Ok(self.add_uncomplete_node(node, UnlinkedNodeBody::VariableValue { ident, path }))
            }
            Dot => {
                let path = self.find_traverse_path(&node)?;
                self.do_expression_symbol(node, ident, Some(path))
            }
            token => {
                let token = token.clone();
                Err(wrong_token_error(
                    &self.ast,
                    node.id,
                    &token,
                    &[
                        &LeftBrace,
                        &RightBrace,
                        &EndStatement,
                        &ListSeparator,
                        &Dot,
                        &Op(ArithmeticOP::Add),
                    ],
                ))
            }
        }
    }

    fn do_function_call(&mut self, node: PendingNode, ident: String) -> Result {
        use crate::token::TokenType::*;

        self.next_token(&node).expect(&LeftBrace)?;
        let mut args = Vec::new();
        while self.peek_token().not(&RightBrace)? {
            let arg = self.do_expression(node.id)?;
            args.push(arg);
            if self.peek_token().is(&ListSeparator)? {
                self.eat_token(&node);
            }
        }
        self.next_token(&node).expect(&RightBrace)?;
        Ok(self.add_uncomplete_node(node, UnlinkedNodeBody::Call { ident, args }))
    }

    fn do_return(&mut self, node: PendingNode) -> Result {
        let expr = match self.peek_token().not(&EndStatement)? {
            true => Some(self.do_expression(node.id)?),
            false => None,
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
