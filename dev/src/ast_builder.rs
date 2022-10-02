use crate::node::{
    EvaluateExpression, ExpressionChain, FunctionCall, FunctionDeclaration, NodeState, Return,
    StaticAssignment, TypeDeclaration, VariableValue,
};
use crate::token::{KeyName, Token};
use crate::{Ast, Block, Error, Node, NodeType, TokenType};
use crate::{
    Expression, NodeID, Operation, Result, State, Statement, Value, Variable, VariableAssignment,
    VariableDeclaration,
};
use std::iter::Peekable;

pub struct AstBuilder<'a, Iter, T>
where
    Iter: Iterator<Item = Token<'a>>,
    T: Node,
{
    tokens: Peekable<Iter>,
    ast: Ast<T>,
}

impl<'a, Iter, T> AstBuilder<'a, Iter, T>
where
    Iter: Iterator<Item = Token<'a>>,
    T: Node,
{
    pub fn new(tokens: Iter) -> Self {
        Self {
            tokens: tokens.peekable(),
            ast: Ast::default(),
        }
    }
}

impl<'a, Iter> AstBuilder<'a, Iter, Block>
where
    Iter: Iterator<Item = Token<'a>>,
{
    pub fn build(mut self) -> Result<Ast<Block>> {
        let block_id = self.ast.new_root_node();
        let mut children = vec![];
        while let Some(statement) = self.statement(block_id)? {
            children.push(statement);
        }
        let block = Block::new(children, &self.ast);
        let block_id = self.ast.push(block_id, block);
        self.ast.root = Some(block_id);
        Ok(self.ast)
    }

    fn statement(&mut self, parent_id: NodeID<Block>) -> Result<Option<NodeID<Statement>>> {
        loop {
            if let Ok(token_tp) = self.peek_token_type() {
                match token_tp {
                    TokenType::Name(_) => {
                        if let TokenType::Name(name) = self.tokens.next().unwrap().tp {
                            let statement = self.statement_named(name, parent_id);
                            break Some(statement).transpose();
                        }
                        unreachable!()
                    }
                    TokenType::KeyName(KeyName::Return) => {
                        self.next_token()?;
                        let return_id = self.ast.new_node(parent_id);
                        let value = match self.peek_token_type()? {
                            TokenType::EndStatement => {
                                self.expect_end_statement()?;
                                None
                            }
                            _ => {
                                let expr = self.expression(return_id, None)?;
                                self.expect_end_statement()?;
                                Some(expr)
                            }
                        };
                        let return_value = self.ast.push(
                            return_id,
                            Return {
                                func: ().into(),
                                value,
                            },
                        );
                        break Ok(Some(return_value.into()));
                    }
                    TokenType::Comment(_) => {
                        self.next_token()?;
                    }
                    _ => break Ok(None),
                }
            } else {
                break Ok(None);
            }
        }
    }

    fn expect_end_statement(&mut self) -> Result<()> {
        let token = self.next_token()?;
        if let TokenType::EndStatement = token.tp {
            Ok(())
        } else {
            Err(Error::ExpectedEndStatement)
        }
    }

    fn peek_token(&mut self) -> Result<&Token> {
        self.tokens.peek().ok_or(Error::EOF)
    }

    fn peek_token_type(&mut self) -> Result<&TokenType> {
        self.peek_token().map(|token| &token.tp)
    }

    fn next_token(&mut self) -> Result<Token> {
        loop {
            let token = self.tokens.next().ok_or(Error::EOF)?;
            match token.tp {
                TokenType::Comment(_) => continue,
                _ => break Ok(token),
            }
        }
    }

    fn call(&mut self, variable: NodeID<VariableValue>) -> Result<FunctionCall> {
        assert_eq!(TokenType::LeftBrace, self.next_token()?.tp);
        assert_eq!(TokenType::RightBrace, self.next_token()?.tp);

        Ok(FunctionCall::new(variable))
    }

    fn tp(&mut self) -> Result<NodeType> {
        let tp = match self.next_token()?.tp {
            TokenType::Name("Int") => NodeType::Int,
            TokenType::Name("Float") => NodeType::Float,
            TokenType::Name("String") => NodeType::String,
            TokenType::Name("Boolean") => NodeType::Boolean,
            _ => unimplemented!(),
        };
        Ok(tp)
    }

    fn arguments_with_types(
        &mut self,
        parent_id: NodeID<Expression>,
        this: Option<NodeType>,
    ) -> Result<Vec<NodeID<Variable>>> {
        let mut arguments = vec![];

        let mut expect_separator = if let Some(this) = this {
            if let TokenType::KeyName(KeyName::This) = self.peek_token_type()? {
                self.next_token()?;
                let variable = self.ast.push_new_node(
                    parent_id,
                    Variable {
                        name: "self".into(),
                        tp: Some(this),
                    },
                );
                arguments.push(variable);
                true
            } else {
                false
            }
        } else {
            false
        };

        loop {
            match (expect_separator, self.next_token()?.tp) {
                (false, TokenType::Name(arg)) => {
                    let name = arg.into();

                    assert_eq!(TokenType::TypeDeclaration, self.next_token()?.tp);

                    let tp = Some(self.tp()?);

                    let variable = self.ast.push_new_node(parent_id, Variable { name, tp });
                    arguments.push(variable);
                    expect_separator = true;
                }
                (true, TokenType::ListSeparator) => {
                    expect_separator = false;
                }
                (_, TokenType::RightBrace) => break,
                _ => unimplemented!(),
            }
        }
        Ok(arguments)
    }

    fn expression(
        &mut self,
        parent_id: impl Into<NodeID>,
        this: Option<NodeType>,
    ) -> Result<NodeID<Expression>> {
        let parent_id = parent_id.into();
        let expression_id = self.ast.new_node(parent_id);
        let expression: Expression = match self.next_token()?.tp {
            TokenType::Int(value, _) => Value::Int(value).into(),
            TokenType::Float(value, _) => Value::Float(value).into(),
            TokenType::String(value) => Value::String(value.to_string()).into(),
            TokenType::Name(value) => {
                let variable = VariableValue::new(State::Unlinked(value.to_string()));
                match self.peek_token_type()? {
                    TokenType::Dot => {
                        self.next_token()?;

                        let variable = self.ast.push_new_node(expression_id, variable);
                        let rhs = self.expression(expression_id, None)?;

                        ExpressionChain::new(variable.into(), rhs).into()
                    }
                    TokenType::LeftBrace => {
                        let variable = self.ast.push_new_node(expression_id, variable);
                        self.call(variable)?.into()
                    }
                    _ => variable.into(),
                }
            }
            TokenType::KeyName(KeyName::Fn) => {
                let arguments = if let TokenType::LeftBrace = self.peek_token_type()? {
                    self.next_token()?;
                    self.arguments_with_types(expression_id, this)?
                } else {
                    vec![]
                };

                let returns = if let TokenType::ReturnTypes = self.peek_token_type()? {
                    self.next_token()?;
                    self.tp()?
                } else {
                    NodeType::Void
                };

                assert_eq!(TokenType::LeftCurlyBrace, self.next_token()?.tp);
                let body_id = self.ast.new_node(expression_id);
                let mut children = vec![];
                loop {
                    if let TokenType::RightCurlyBrace = self.peek_token_type()? {
                        self.next_token()?;
                        break;
                    }
                    match self.statement(body_id)? {
                        None => break,
                        Some(statement) => children.push(statement),
                    }
                }
                let body = self.ast.push(body_id, Block::new(children, &self.ast));

                FunctionDeclaration {
                    arguments,
                    returns,
                    body,
                }
                .into()
            }
            tp => unimplemented!("{:?}", tp),
        };

        let lhs = self.ast.push(expression_id, expression);

        match self.peek_token_type()? {
            TokenType::Op(op) => {
                let op = *op;
                self.next_token()?;

                let operation_id = self.ast.new_node(parent_id);
                let rhs = self.expression(operation_id, None)?;
                let operation = Expression::Operation(Operation::new(op, lhs, rhs));
                let operation = self.ast.push(operation_id, operation);

                let lhs_node = self.ast.get_mut(lhs);
                lhs_node.parent_id = Some(operation.into());

                Ok(operation)
            }
            _ => Ok(lhs),
        }
    }

    fn new_type(
        &mut self,
        statement_id: NodeID<TypeDeclaration>,
        variable: NodeID<Variable>,
    ) -> Result<NodeID<TypeDeclaration>> {
        let expr_id = self.ast.new_node(variable);
        let body = self
            .ast
            .push_new_node(expr_id, Block::new(vec![], &self.ast));

        let expr = FunctionDeclaration {
            arguments: vec![],
            body,
            returns: NodeType::Void,
        };
        let expr = self.ast.push(expr_id, expr);

        match self.next_token()?.tp {
            TokenType::LeftCurlyBrace => match self.next_token()?.tp {
                TokenType::RightCurlyBrace => {}
                _ => unimplemented!(),
            },
            _ => unimplemented!(),
        }

        let declaration = TypeDeclaration::new(variable, expr.into());
        let type_declaration = self.ast.push(statement_id, declaration);

        self.ast.get_inner_mut(expr).returns = NodeType::Custom(type_declaration);

        Ok(type_declaration)
    }

    fn statement_named(
        &mut self,
        name: &str,
        parent_id: NodeID<Block>,
    ) -> Result<NodeID<Statement>> {
        let next_token = self.next_token()?;
        let id = match next_token.tp {
            TokenType::VariableDeclaration => {
                let statement_id = self.ast.new_node(parent_id);

                let variable = self
                    .ast
                    .push_new_node(statement_id, Variable::new(name.to_string()));

                let value = self.expression(variable, None)?;
                self.expect_end_statement()?;

                let statement = VariableDeclaration::new(variable, value);
                self.ast.push(statement_id, statement).into()
            }
            TokenType::Assignment => {
                let statement_id = self.ast.new_node(parent_id);

                let value = self.expression(statement_id, None)?;
                self.expect_end_statement()?;

                let statement = VariableAssignment::new(name.to_string(), value);
                self.ast.push(statement_id, statement).into()
            }
            TokenType::ReturnTypes => {
                let statement_id = self.ast.new_node(parent_id);

                match self.next_token()?.tp {
                    TokenType::KeyName(KeyName::Type) => {}
                    _ => unimplemented!(),
                }
                let variable = self
                    .ast
                    .push_new_node(statement_id, Variable::new(name.to_string()));

                self.new_type(statement_id, variable)?.into()
            }
            TokenType::Dot => {
                let field = match self.next_token()?.tp {
                    TokenType::Name(path) => path.to_string(),
                    _ => unimplemented!(),
                };

                match self.peek_token_type()? {
                    TokenType::ConstDeclaration => {
                        self.next_token()?;
                        let statement_id = self.ast.new_node(parent_id);

                        let assign_to = self.ast.push_new_node(
                            statement_id,
                            NodeState {
                                state: name.to_string().into(),
                            },
                        );

                        let variable = self.ast.push_new_node(statement_id, Variable::new(field));
                        let this = Some(NodeType::Indirect(assign_to));

                        let value = self.expression(statement_id, this)?;
                        self.expect_end_statement()?;

                        let statement = StaticAssignment {
                            assign_to,
                            variable,
                            value,
                            is_associated_field: false,
                        };
                        self.ast.push(statement_id, statement).into()
                    }
                    TokenType::LeftBrace => {
                        let statement_id = self.ast.new_node(parent_id);

                        let chain_id = self.ast.new_node(statement_id);

                        let variable = self.ast.push_new_node(
                            statement_id,
                            VariableValue::new(State::Unlinked(name.to_string())),
                        );

                        let call_id = self.ast.new_node(chain_id);
                        let field = self
                            .ast
                            .push_new_node(call_id, VariableValue::new(State::Unlinked(field)));

                        let call = self.call(field)?;
                        let call = self.ast.push(call_id, call).into();
                        self.expect_end_statement()?;

                        let chain = self
                            .ast
                            .push(chain_id, ExpressionChain::new(variable.into(), call));

                        let statement = EvaluateExpression {
                            value: chain.into(),
                        };
                        self.ast.push(statement_id, statement).into()
                    }
                    _ => unimplemented!(),
                }
            }
            tt => unimplemented!("{:?}", tt),
        };
        Ok(id)
    }
}
