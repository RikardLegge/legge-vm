use crate::node::{
    AstRootNode, Break, EvaluateExpression, ExpressionChain, FunctionCall, FunctionDeclaration, If,
    Loop, Return, StaticAssignment, TypeDeclaration, VariableValue,
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
    tokens: TokenReader<'a, Iter>,
    ast: Ast<T>,
}

impl<'a, Iter, T> AstBuilder<'a, Iter, T>
where
    Iter: Iterator<Item = Token<'a>>,
    T: Node,
{
    pub fn new(tokens: Iter) -> Self {
        Self {
            tokens: TokenReader {
                tokens: tokens.peekable(),
            },
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

        let mut builder = Builder::new(&mut self.ast, &mut self.tokens, block_id);
        loop {
            match builder.statement() {
                Ok(statement) => {
                    children.push(statement);
                }
                Err(Error::EOF) => break,
                Err(err) => return Err(err),
            }
        }

        let block = Block::new(children, &self.ast);
        let block_id = self.ast.push(block_id, block);
        self.ast.root = Some(block_id);
        Ok(self.ast)
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

struct Builder<'ast, 'token, Type, TokenIter>
where
    NodeID<Type>: Into<NodeID>,
    Type: Node + Into<AstRootNode>,
    TokenIter: Iterator<Item = Token<'token>>,
{
    id: NodeID<Type>,
    ast: &'ast mut Ast,
    tokens: &'ast mut TokenReader<'token, TokenIter>,
}

impl<'ast, 'token, Type, TokenIter> Builder<'ast, 'token, Type, TokenIter>
where
    NodeID<Type>: Into<NodeID>,
    Type: Node + Into<AstRootNode>,
    TokenIter: Iterator<Item = Token<'token>>,
{
    fn new(
        ast: &'ast mut Ast,
        tokens: &'ast mut TokenReader<'token, TokenIter>,
        parent_id: NodeID<Type>,
    ) -> Self {
        let id = parent_id;
        Builder { id, ast, tokens }
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
                let break_value = self.node::<Break>(|mut builder| {
                    let value = match builder.tokens.peek_type()? {
                        TokenType::EndStatement => None,
                        _ => Some(builder.expression(None)?),
                    };
                    builder.tokens.expect_end_statement()?;

                    Ok(Break {
                        r#loop: ().into(),
                        value,
                    })
                })?;

                Ok(break_value.into())
            }
            TokenType::KeyName(KeyName::Return) => {
                self.tokens.next()?;
                let return_value = self.node::<Return>(|mut builder| {
                    let value = match builder.tokens.peek_type()? {
                        TokenType::EndStatement => None,
                        _ => Some(builder.expression(None)?),
                    };
                    builder.tokens.expect_end_statement()?;

                    Ok(Return {
                        func: ().into(),
                        value,
                    })
                })?;

                Ok(return_value.into())
            }
            _ => {
                let expression = self.node::<EvaluateExpression>(|mut builder| {
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

                    Ok(EvaluateExpression {
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
            TokenType::Int(value, _) => self.const_value(Value::Int(value)).into(),
            TokenType::Float(value, _) => self.const_value(Value::Float(value)).into(),
            TokenType::String(value) => self.const_value(Value::String(value.to_string())).into(),
            TokenType::Name(value) => match self.tokens.peek_type()? {
                TokenType::Dot => {
                    self.tokens.next()?;
                    self.node::<ExpressionChain>(move |mut builder| {
                        let lhs = builder.variable_value(value.to_string()).into();
                        let rhs = builder.expression(None)?;

                        Ok(ExpressionChain::new(lhs, rhs))
                    })?
                    .into()
                }
                TokenType::LeftBrace => self.call(value.to_string())?.into(),
                _ => self.variable_value(value.to_string()).into(),
            },
            TokenType::LeftCurlyBrace => self
                .node::<Block>(|mut builder| {
                    let mut children = vec![];
                    loop {
                        if let TokenType::RightCurlyBrace = builder.tokens.peek_type()? {
                            break;
                        }
                        let statement = builder.statement()?;
                        children.push(statement);
                    }

                    assert_eq!(TokenType::RightCurlyBrace, builder.tokens.next()?.tp);

                    Ok(Block::new(children, builder.ast))
                })?
                .into(),
            TokenType::KeyName(KeyName::Fn) => self
                .node::<FunctionDeclaration>(|mut builder| {
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

                    let body = builder.node::<Block>(move |mut builder| {
                        let mut children = vec![];
                        loop {
                            if let TokenType::RightCurlyBrace = builder.tokens.peek_type()? {
                                break;
                            }
                            let statement = builder.statement()?;
                            children.push(statement);
                        }
                        Ok(Block::new(children, builder.ast))
                    })?;

                    assert_eq!(TokenType::RightCurlyBrace, builder.tokens.next()?.tp);

                    Ok(FunctionDeclaration {
                        arguments,
                        returns,
                        body,
                    })
                })?
                .into(),
            TokenType::KeyName(KeyName::Loop) => self
                .node::<Loop>(|mut builder| {
                    assert_eq!(TokenType::LeftCurlyBrace, builder.tokens.next()?.tp);

                    let body = builder.node::<Block>(move |mut builder| {
                        let mut children = vec![];
                        loop {
                            if let TokenType::RightCurlyBrace = builder.tokens.peek_type()? {
                                break;
                            }
                            let statement = builder.statement()?;
                            children.push(statement);
                        }
                        Ok(Block::new(children, builder.ast))
                    })?;
                    let value = None;

                    assert_eq!(TokenType::RightCurlyBrace, builder.tokens.next()?.tp);

                    Ok(Loop { body, value })
                })?
                .into(),
            TokenType::KeyName(KeyName::If) => self
                .node::<If>(|mut builder| {
                    let cond = builder.expression(None)?;

                    assert_eq!(TokenType::LeftCurlyBrace, builder.tokens.next()?.tp);

                    let body = builder.node::<Block>(move |mut builder| {
                        let mut children = vec![];
                        loop {
                            if let TokenType::RightCurlyBrace = builder.tokens.peek_type()? {
                                break;
                            }
                            let statement = builder.statement()?;
                            children.push(statement);
                        }
                        Ok(Block::new(children, builder.ast))
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

                    Ok(If { cond, body, r#else })
                })?
                .into(),
            tp => unimplemented!("{:?}", tp),
        };

        match self.tokens.peek_type()? {
            TokenType::Op(op) => {
                let op = *op;
                self.tokens.next()?;

                let operation = self.node::<Operation>(|mut builder| {
                    let rhs = builder.expression(None)?;
                    Ok(Operation::new(op, lhs, rhs))
                })?;

                let lhs_node = self.ast.get_mut(lhs);
                lhs_node.parent_id = Some(operation.into());

                Ok(operation.into())
            }
            _ => Ok(lhs),
        }
    }

    fn call(&mut self, name: String) -> Result<NodeID<FunctionCall>> {
        self.node::<FunctionCall>(|mut builder| {
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

            Ok(FunctionCall::new(variable, args))
        })
    }

    fn variable(&mut self, name: String) -> NodeID<Variable> {
        let parent_id = self.id.into();
        let variable = Variable::new(name);
        self.ast.push_new_node(parent_id, variable)
    }

    fn typed_variable(&mut self, name: String, tp: NodeType) -> NodeID<Variable> {
        let parent_id = self.id.into();
        let variable = Variable { name, tp: Some(tp) };
        self.ast.push_new_node(parent_id, variable)
    }

    fn variable_value(&mut self, name: String) -> NodeID<VariableValue> {
        let parent_id = self.id.into();
        let variable = VariableValue::new(State::Unlinked(name));
        self.ast.push_new_node(parent_id, variable)
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

    fn node<ChildNodeType>(
        &mut self,
        evaluate: impl FnOnce(Builder<'_, 'token, ChildNodeType, TokenIter>) -> Result<ChildNodeType>,
    ) -> Result<NodeID<ChildNodeType>>
    where
        ChildNodeType: Node + Into<AstRootNode>,
    {
        let id = self.ast.new_node(self.id);
        let builder = Builder::<ChildNodeType, TokenIter> {
            id,
            ast: self.ast,
            tokens: self.tokens,
        };
        let value = evaluate(builder)?;
        Ok(self.ast.push(id, value))
    }

    fn const_value<ChildNodeType>(&mut self, value: ChildNodeType) -> NodeID<ChildNodeType>
    where
        ChildNodeType: Node + Into<AstRootNode>,
    {
        let id = self.ast.new_node(self.id);
        self.ast.push(id, value)
    }

    fn type_constructor(
        &mut self,
        tp_id: NodeID<TypeDeclaration>,
    ) -> Result<NodeID<FunctionDeclaration>> {
        self.node::<FunctionDeclaration>(move |mut builder| {
            let body = builder.node::<Block>(|mut builder| {
                let ret = builder
                    .node::<Return>(|mut builder| {
                        let value = builder.const_value(Value::Custom(tp_id));
                        Ok(Return {
                            func: ().into(),
                            value: Some(value.into()),
                        })
                    })?
                    .into();
                Ok(Block::new(vec![ret], builder.ast))
            })?;
            Ok(FunctionDeclaration {
                arguments: vec![],
                body,
                returns: NodeType::Custom(tp_id),
            })
        })
    }

    fn statement_named(&mut self, name: String) -> Result<NodeID<Statement>> {
        let id = match self.tokens.peek_type()? {
            TokenType::VariableDeclaration => self
                .node::<VariableDeclaration>(move |mut builder| {
                    builder.tokens.next()?;
                    let variable = builder.variable(name);
                    let value = builder.expression(None)?;
                    Ok(VariableDeclaration::new(variable, value, false))
                })?
                .into(),
            TokenType::ConstDeclaration => self
                .node::<VariableDeclaration>(move |mut builder| {
                    builder.tokens.next()?;
                    let variable = builder.variable(name);
                    let value = builder.expression(None)?;
                    Ok(VariableDeclaration::new(variable, value, true))
                })?
                .into(),
            TokenType::Assignment => self
                .node::<VariableAssignment>(move |mut builder| {
                    builder.tokens.next()?;
                    let value = builder.expression(None)?;
                    Ok(VariableAssignment::new(name, value))
                })?
                .into(),
            TokenType::LeftBrace => self
                .node::<EvaluateExpression>(|mut builder| {
                    let call = builder.call(name)?;
                    Ok(EvaluateExpression::new(call))
                })?
                .into(),
            TokenType::ReturnTypes => self
                .node::<TypeDeclaration>(move |mut builder| {
                    builder.tokens.next()?;
                    let variable = builder.variable(name);

                    assert_eq!(
                        builder.tokens.next_type()?,
                        TokenType::KeyName(KeyName::Type)
                    );

                    let constructor = builder.type_constructor(builder.id)?;

                    assert_eq!(builder.tokens.next_type()?, TokenType::LeftCurlyBrace);
                    assert_eq!(builder.tokens.next_type()?, TokenType::RightCurlyBrace);

                    Ok(TypeDeclaration::new(variable, constructor.into()))
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

                        self.node::<StaticAssignment>(move |mut builder| {
                            let assign_to = builder.variable_value(name);
                            let variable = builder.variable(field);

                            let this = Some(NodeType::Indirect(assign_to));
                            let value = builder.expression(this)?;

                            Ok(StaticAssignment::new(assign_to, variable, value))
                        })?
                        .into()
                    }
                    TokenType::LeftBrace => self
                        .node::<EvaluateExpression>(move |mut builder| {
                            let chain = builder.node::<ExpressionChain>(move |mut builder| {
                                let variable = builder.variable_value(name);
                                let call = builder.call(field)?;

                                Ok(ExpressionChain::new(variable.into(), call.into()))
                            })?;
                            Ok(EvaluateExpression::new(chain))
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
