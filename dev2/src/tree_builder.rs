use std::iter::Peekable;

use crate::ast::{NodeBody, NodeData, NodeDataStorage};
use crate::node::{
    Any, Ast, Block, BlockStorage, BreakStorage, Error, EvaluateExpressionStorage, Expression,
    ExpressionChainStorage, FunctionCall, FunctionCallStorage, FunctionDeclaration,
    FunctionDeclarationStorage, IfStorage, LoopStorage, NodeID, OperationStorage, Result,
    ReturnStorage, Statement, StaticAssignmentStorage, TypeDeclaration, TypeDeclarationStorage,
    ValueStorage, Variable, VariableAssignmentStorage, VariableDeclarationStorage, VariableStorage,
    VariableValue, VariableValueStorage,
};
use crate::state::State;
use crate::token::{KeyName, Token, TokenType};
use crate::types::NodeType;

pub fn from_tokens(tokens: Vec<Token>) -> Result<(Ast, NodeID<Block>)> {
    TreeBuilder::new(tokens.into_iter()).build()
}

pub struct TreeBuilder<'a, Iter>
where
    Iter: Iterator<Item = Token<'a>>,
{
    tokens: TokenReader<'a, Iter>,
    ast: Ast,
}

impl<'a, Iter> TreeBuilder<'a, Iter>
where
    Iter: Iterator<Item = Token<'a>>,
{
    pub fn new(tokens: Iter) -> Self {
        Self {
            tokens: TokenReader {
                tokens: tokens.peekable(),
            },
            ast: Ast::new(),
        }
    }
}

impl<'a, Iter> TreeBuilder<'a, Iter>
where
    Iter: Iterator<Item = Token<'a>>,
{
    pub fn build(mut self) -> Result<(Ast, NodeID<Block>)> {
        let block_id = self
            .ast
            .node_body(None, BlockStorage::new(Vec::new(), &self.ast));
        let mut children = vec![];

        let mut builder = Builder::new::<BlockStorage>(&mut self.ast, &mut self.tokens, block_id);
        loop {
            match builder.statement() {
                Ok(statement) => {
                    children.push(statement);
                }
                Err(Error::EOF) => break,
                Err(err) => return Err(err),
            }
        }

        let variables = BlockStorage::variables(&children, &self.ast);
        self.ast.body_mut(block_id).set(children, variables);
        Ok((self.ast, block_id))
    }
}

pub struct TokenReader<'a, Iter>
where
    Iter: Iterator<Item = Token<'a>>,
{
    tokens: Peekable<Iter>,
}

impl<'a, Iter> TokenReader<'a, Iter>
where
    Iter: Iterator<Item = Token<'a>>,
{
    fn expect_end_statement(&mut self) -> Result<()> {
        match self.next_type()? {
            TokenType::EndStatement => Ok(()),
            _ => {
                panic!();
                // Err(Error::ExpectedEndStatement)
            }
        }
    }

    fn peek(&mut self) -> Result<&Token<'a>> {
        loop {
            let token = self.tokens.peek().ok_or(Error::EOF)?;
            match &token.tp {
                TokenType::Comment(_) => {
                    self.tokens.next().unwrap();
                    continue;
                }
                _ => return Ok(self.tokens.peek().unwrap()),
            }
        }
    }

    fn peek_type(&mut self) -> Result<&TokenType<'a>> {
        self.peek().map(|token| &token.tp)
    }

    fn next_type(&mut self) -> Result<TokenType<'a>> {
        self.next().map(|token| token.tp)
    }

    fn next(&mut self) -> Result<Token<'a>> {
        self.peek()?;
        self.tokens.next().ok_or(Error::EOF)
    }
}

struct Builder<'ast, 'token, TokenIter, Body: NodeBody>
where
    TokenIter: Iterator<Item = Token<'token>>,
    NodeID<Body>: Into<NodeID>,
{
    id: NodeID<Body>,
    ast: &'ast mut Ast,
    tokens: &'ast mut TokenReader<'token, TokenIter>,
}

impl<'ast, 'token, TokenIter, Body: NodeBody> Builder<'ast, 'token, TokenIter, Body>
where
    TokenIter: Iterator<Item = Token<'token>>,
    NodeID<Body>: Into<NodeID>,
{
    fn new<T: NodeData<Node = Body>>(
        ast: &'ast mut Ast,
        tokens: &'ast mut TokenReader<'token, TokenIter>,
        parent_id: impl Into<NodeID<Body>>,
    ) -> Self {
        let id = parent_id.into();
        Self { id, ast, tokens }
    }

    fn statement(&mut self) -> Result<NodeID<Statement>> {
        match self.tokens.peek_type()? {
            TokenType::Name(_) => {
                if let Ok(TokenType::Name(name)) = self.tokens.next_type() {
                    self.statement_named(name.to_string())
                } else {
                    unreachable!()
                }
            }
            TokenType::KeyName(KeyName::Break) => {
                self.tokens.next()?;
                let break_value = self.node::<BreakStorage>(|mut builder| {
                    let value = match builder.tokens.peek_type()? {
                        TokenType::EndStatement => None,
                        _ => Some(builder.expression(None)?),
                    };
                    builder.tokens.expect_end_statement()?;

                    Ok(BreakStorage {
                        r#loop: ().into(),
                        value,
                    })
                })?;

                Ok(break_value.into())
            }
            TokenType::KeyName(KeyName::Return) => {
                self.tokens.next()?;
                let return_value = self.node::<ReturnStorage>(|mut builder| {
                    let value = match builder.tokens.peek_type()? {
                        TokenType::EndStatement => None,
                        _ => Some(builder.expression(None)?),
                    };
                    builder.tokens.expect_end_statement()?;

                    Ok(ReturnStorage {
                        func: ().into(),
                        value,
                    })
                })?;

                Ok(return_value.into())
            }
            _ => {
                let expression = self.node::<EvaluateExpressionStorage>(|mut builder| {
                    let value = builder.expression(None)?;

                    let has_end_statement = match builder.tokens.peek_type() {
                        Ok(TokenType::RightCurlyBrace) => false,
                        Ok(TokenType::EndStatement) => {
                            builder.tokens.next()?;
                            true
                        }
                        _ => {
                            builder.tokens.expect_end_statement()?;
                            true
                        }
                    };

                    Ok(EvaluateExpressionStorage {
                        value,
                        has_end_statement,
                    })
                })?;

                Ok(expression.into())
            }
        }
    }

    fn expression(&mut self, this: Option<NodeType>) -> Result<NodeID<Expression>> {
        let lhs: NodeID<Expression> = match self.tokens.next_type()? {
            TokenType::Int(value, _) => self
                .const_value::<ValueStorage>(ValueStorage::Int(value))
                .into(),
            TokenType::Float(value, _) => self
                .const_value::<ValueStorage>(ValueStorage::Float(value))
                .into(),
            TokenType::String(value) => self
                .const_value::<ValueStorage>(ValueStorage::String(value.to_string()))
                .into(),
            TokenType::Name(value) => match self.tokens.peek_type()? {
                TokenType::Dot => {
                    self.tokens.next()?;
                    self.node::<ExpressionChainStorage>(move |mut builder| {
                        let lhs = builder.variable_value(value.to_string()).into();
                        let rhs = builder.expression(None)?;

                        Ok(ExpressionChainStorage::new(lhs, rhs))
                    })?
                    .into()
                }
                TokenType::LeftBrace => self.call(value.to_string())?.into(),
                _ => self.variable_value(value.to_string()).into(),
            },
            TokenType::LeftCurlyBrace => self
                .node::<BlockStorage>(|mut builder| {
                    let mut children = vec![];
                    loop {
                        if let TokenType::RightCurlyBrace = builder.tokens.peek_type()? {
                            break;
                        }
                        let statement = builder.statement()?;
                        children.push(statement);
                    }

                    assert_eq!(TokenType::RightCurlyBrace, builder.tokens.next()?.tp);

                    Ok(BlockStorage::new(children, builder.ast))
                })?
                .into(),
            TokenType::KeyName(KeyName::Fn) => self
                .node::<FunctionDeclarationStorage>(|mut builder| {
                    let arguments = if let TokenType::LeftBrace = builder.tokens.peek_type()? {
                        builder.arguments_with_types(this)?
                    } else {
                        vec![]
                    };

                    let returns = if let TokenType::ReturnTypes = builder.tokens.peek_type()? {
                        builder.tokens.next()?;
                        builder.tp()?
                    } else {
                        NodeType::Void
                    };

                    assert_eq!(TokenType::LeftCurlyBrace, builder.tokens.next()?.tp);

                    let body = builder.node::<BlockStorage>(move |mut builder| {
                        let mut children = vec![];
                        loop {
                            if let TokenType::RightCurlyBrace = builder.tokens.peek_type()? {
                                break;
                            }
                            let statement = builder.statement()?;
                            children.push(statement);
                        }
                        Ok(BlockStorage::new(children, builder.ast))
                    })?;

                    assert_eq!(TokenType::RightCurlyBrace, builder.tokens.next()?.tp);

                    Ok(FunctionDeclarationStorage {
                        arguments,
                        returns,
                        body,
                    })
                })?
                .into(),
            TokenType::KeyName(KeyName::Loop) => self
                .node::<LoopStorage>(|mut builder| {
                    assert_eq!(TokenType::LeftCurlyBrace, builder.tokens.next()?.tp);

                    let body = builder.node::<BlockStorage>(move |mut builder| {
                        let mut children = vec![];
                        loop {
                            if let TokenType::RightCurlyBrace = builder.tokens.peek_type()? {
                                break;
                            }
                            let statement = builder.statement()?;
                            children.push(statement);
                        }
                        Ok(BlockStorage::new(children, builder.ast))
                    })?;
                    let value = None;

                    assert_eq!(TokenType::RightCurlyBrace, builder.tokens.next()?.tp);

                    Ok(LoopStorage { body, value })
                })?
                .into(),
            TokenType::KeyName(KeyName::If) => self
                .node::<IfStorage>(|mut builder| {
                    let cond = builder.expression(None)?;

                    assert_eq!(TokenType::LeftCurlyBrace, builder.tokens.next()?.tp);

                    let body = builder.node::<BlockStorage>(move |mut builder| {
                        let mut children = vec![];
                        loop {
                            if let TokenType::RightCurlyBrace = builder.tokens.peek_type()? {
                                break;
                            }
                            let statement = builder.statement()?;
                            children.push(statement);
                        }
                        Ok(BlockStorage::new(children, builder.ast))
                    })?;

                    assert_eq!(TokenType::RightCurlyBrace, builder.tokens.next()?.tp);

                    let r#else = match builder.tokens.peek_type()? {
                        &TokenType::KeyName(KeyName::Else) => {
                            builder.tokens.next()?;
                            let expr = builder.expression(None)?;
                            Some(expr)
                        }
                        _ => None,
                    };

                    Ok(IfStorage { cond, body, r#else })
                })?
                .into(),
            tp => unimplemented!("{:?}", tp),
        };

        match self.tokens.peek_type()? {
            TokenType::Op(op) => {
                let op = *op;
                self.tokens.next()?;

                let operation = self.node::<OperationStorage>(|mut builder| {
                    let rhs = builder.expression(None)?;
                    Ok(OperationStorage::new(op, lhs, rhs))
                })?;

                let lhs_node = self.ast.get_mut::<Expression>(lhs);
                lhs_node.parent_id = Some(operation.into());

                Ok(operation.into())
            }
            _ => Ok(lhs),
        }
    }

    fn call(&mut self, name: String) -> Result<NodeID<FunctionCall>> {
        self.node::<FunctionCallStorage>(|mut builder| {
            let variable = builder.variable_value(name);

            assert_eq!(TokenType::LeftBrace, builder.tokens.next()?.tp);

            let mut args = vec![];
            loop {
                if let TokenType::RightBrace = builder.tokens.peek_type()? {
                    break;
                }

                let arg = builder.expression(None)?;
                args.push(arg);

                if let TokenType::ListSeparator = builder.tokens.peek_type()? {
                    builder.tokens.next_type()?;
                }
            }

            assert_eq!(TokenType::RightBrace, builder.tokens.next()?.tp);

            Ok(FunctionCallStorage::new(variable, args))
        })
    }

    fn variable(&mut self, name: String) -> NodeID<Variable> {
        let parent_id = self.id.into();
        self.ast
            .node_body::<VariableStorage>(Some(parent_id), VariableStorage::new(name))
    }

    fn typed_variable(&mut self, name: String, tp: NodeType) -> NodeID<Variable> {
        let parent_id = self.id.into();
        self.ast
            .node_body::<VariableStorage>(Some(parent_id), VariableStorage { name, tp: Some(tp) })
    }

    fn variable_value(&mut self, name: String) -> NodeID<VariableValue> {
        let parent_id = self.id.into();
        self.ast.node_body::<VariableValueStorage>(
            Some(parent_id),
            VariableValueStorage::new(State::Unlinked(name)),
        )
    }

    fn tp(&mut self) -> Result<NodeType> {
        let tp = match self.tokens.next_type()? {
            TokenType::Name("Int") => NodeType::Int,
            TokenType::Name("Float") => NodeType::Float,
            TokenType::Name("String") => NodeType::String,
            TokenType::Name("Boolean") => NodeType::Boolean,
            _ => unimplemented!(),
        };
        Ok(tp)
    }

    fn arguments_with_types(&mut self, this: Option<NodeType>) -> Result<Vec<NodeID<Variable>>> {
        assert_eq!(self.tokens.next_type()?, TokenType::LeftBrace);

        let mut arguments = vec![];

        if let Some(this) = this {
            if let TokenType::KeyName(KeyName::This) = self.tokens.peek_type()? {
                self.tokens.next()?;
                let variable = self.typed_variable("self".to_string(), this);
                arguments.push(variable);

                if let TokenType::ListSeparator = self.tokens.peek_type()? {
                    self.tokens.next()?;
                }
            }
        };

        loop {
            if let TokenType::RightBrace = self.tokens.peek_type()? {
                break;
            }

            if let TokenType::Name(name) = self.tokens.peek_type()? {
                let name = name.to_string();
                assert_eq!(TokenType::TypeDeclaration, self.tokens.next()?.tp);

                let tp = self.tp()?;
                let variable = self.typed_variable(name, tp);

                arguments.push(variable);

                if let TokenType::ListSeparator = self.tokens.peek_type()? {
                    self.tokens.next()?;
                }
            }
        }

        assert_eq!(self.tokens.next_type()?, TokenType::RightBrace);

        Ok(arguments)
    }

    fn node<T: NodeData>(
        &mut self,
        evaluate: impl FnOnce(Builder<'_, 'token, TokenIter, T::Node>) -> Result<T>,
    ) -> Result<NodeID<T::Node>>
    where
        T: Into<<Any as NodeDataStorage>::Storage>,
        NodeID<T::Node>: Into<NodeID<Any>>,
    {
        self.ast.node::<T, Error>(Some(self.id.into()), |ast, id| {
            let builder = Builder {
                id,
                ast,
                tokens: self.tokens,
            };
            evaluate(builder)
        })
    }

    fn const_value<T: NodeData>(&mut self, value: T) -> NodeID<T::Node>
    where
        T: Into<<Any as NodeDataStorage>::Storage>,
        NodeID<T::Node>: Into<NodeID<Any>>,
    {
        self.ast.node_body(Some(self.id.into()), value)
    }

    fn type_constructor(
        &mut self,
        tp_id: NodeID<TypeDeclaration>,
    ) -> Result<NodeID<FunctionDeclaration>> {
        self.node::<FunctionDeclarationStorage>(move |mut builder| {
            let body = builder.node::<BlockStorage>(|mut builder| {
                let ret = builder
                    .node::<ReturnStorage>(|mut builder| {
                        let value =
                            builder.const_value::<ValueStorage>(ValueStorage::Custom(tp_id));
                        Ok(ReturnStorage {
                            func: ().into(),
                            value: Some(value.into()),
                        })
                    })?
                    .into();
                Ok(BlockStorage::new(vec![ret], builder.ast))
            })?;
            Ok(FunctionDeclarationStorage {
                arguments: vec![],
                body,
                returns: NodeType::Custom(tp_id),
            })
        })
    }

    fn statement_named(&mut self, name: String) -> Result<NodeID<Statement>> {
        let id = match self.tokens.peek_type()? {
            TokenType::VariableDeclaration => self
                .node::<VariableDeclarationStorage>(move |mut builder| {
                    builder.tokens.next()?;
                    let variable = builder.variable(name);
                    let value = builder.expression(None)?;
                    Ok(VariableDeclarationStorage::new(variable, value, false))
                })?
                .into(),
            TokenType::ConstDeclaration => self
                .node::<VariableDeclarationStorage>(move |mut builder| {
                    builder.tokens.next()?;
                    let variable = builder.variable(name);
                    let value = builder.expression(None)?;
                    Ok(VariableDeclarationStorage::new(variable, value, true))
                })?
                .into(),
            TokenType::Assignment => self
                .node::<VariableAssignmentStorage>(move |mut builder| {
                    builder.tokens.next()?;
                    let value = builder.expression(None)?;
                    Ok(VariableAssignmentStorage::new(name, value))
                })?
                .into(),
            TokenType::LeftBrace => self
                .node::<EvaluateExpressionStorage>(|mut builder| {
                    let call = builder.call(name)?;
                    Ok(EvaluateExpressionStorage::new(call.into()))
                })?
                .into(),
            TokenType::ReturnTypes => self
                .node::<TypeDeclarationStorage>(move |mut builder| {
                    builder.tokens.next()?;
                    let variable = builder.variable(name);

                    assert_eq!(
                        builder.tokens.next_type()?,
                        TokenType::KeyName(KeyName::Type)
                    );

                    let constructor = builder.type_constructor(builder.id)?;

                    assert_eq!(builder.tokens.next_type()?, TokenType::LeftCurlyBrace);
                    assert_eq!(builder.tokens.next_type()?, TokenType::RightCurlyBrace);

                    Ok(TypeDeclarationStorage::new(variable, constructor.into()))
                })?
                .into(),
            TokenType::Dot => {
                self.tokens.next()?;
                let field = match self.tokens.next_type()? {
                    TokenType::Name(path) => path.to_string(),
                    _ => unimplemented!(),
                };

                match self.tokens.peek_type()? {
                    TokenType::ConstDeclaration => {
                        self.tokens.next()?;

                        self.node::<StaticAssignmentStorage>(move |mut builder| {
                            let assign_to = builder.variable_value(name);
                            let variable = builder.variable(field);

                            let this = Some(NodeType::Indirect(assign_to));
                            let value = builder.expression(this)?;

                            Ok(StaticAssignmentStorage::new(assign_to, variable, value))
                        })?
                        .into()
                    }
                    TokenType::LeftBrace => self
                        .node::<EvaluateExpressionStorage>(move |mut builder| {
                            let chain =
                                builder.node::<ExpressionChainStorage>(move |mut builder| {
                                    let variable = builder.variable_value(name);
                                    let call = builder.call(field)?;

                                    Ok(ExpressionChainStorage::new(variable.into(), call.into()))
                                })?;
                            Ok(EvaluateExpressionStorage::new(chain.into()))
                        })?
                        .into(),
                    _ => unimplemented!(),
                }
            }
            tt => unimplemented!("{:?}", tt),
        };
        self.tokens.expect_end_statement()?;
        Ok(id)
    }
}
