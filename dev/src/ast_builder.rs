use crate::node::{
    ExpressionChain, FunctionCall, FunctionDeclaration, StaticAssignment, TypeDeclaration,
    VariableValue,
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
        self.ast.root = Some(block_id);
        let mut children = vec![];
        while let Some(token) = self.tokens.next() {
            match token.tp {
                TokenType::Name(name) => {
                    let statement = self.statement_named(name, block_id.into())?;
                    children.push(statement);
                }
                TokenType::Comment(_) => continue,
                tt => unimplemented!("{:?}", tt),
            }
        }
        let block = Block::new(children, &self.ast);
        self.ast.push(block_id, block);
        Ok(self.ast)
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
        self.tokens.peek().map(|token| &token.tp).ok_or(Error::EOF)
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

    fn expression(&mut self, parent_id: impl Into<NodeID>) -> Result<NodeID<Expression>> {
        let parent_id = parent_id.into();
        let expression_id = self.ast.new_node(parent_id);
        let next_token = self.next_token()?;
        let expression: Expression = match next_token.tp {
            TokenType::Int(value, _) => Value::Int(value).into(),
            TokenType::Float(value, _) => Value::Float(value).into(),
            TokenType::String(value) => Value::String(value.to_string()).into(),
            TokenType::Name(value) => {
                let variable = VariableValue::new(State::Unlinked(value.to_string()));
                match self.peek_token_type()? {
                    TokenType::Dot => {
                        self.next_token()?;

                        let variable_id = self.ast.new_node(expression_id);
                        let variable = self.ast.push(variable_id, variable);

                        let rhs = self.expression(expression_id)?;
                        ExpressionChain::new(variable.into(), rhs).into()
                    }
                    TokenType::LeftBrace => {
                        self.next_token()?;
                        assert_eq!(TokenType::RightBrace, self.next_token()?.tp);

                        let variable_id = self.ast.new_node(expression_id);
                        let variable = self.ast.push(variable_id, variable);

                        FunctionCall::new(variable).into()
                    }
                    _ => variable.into(),
                }
            }
            tp => unimplemented!("{:?}", tp),
        };

        let lhs = self.ast.push(expression_id, expression);

        match self.peek_token_type()? {
            TokenType::Op(op) => {
                let op = *op;
                self.next_token()?;
                let operation_id = self.ast.new_node(parent_id);

                let rhs = self.expression(operation_id)?;
                let lhs_node = self.ast.get_mut(lhs);
                lhs_node.parent_id = Some(operation_id.into());

                let operation = Expression::Operation(Operation::new(op, lhs, rhs));
                Ok(self.ast.push(operation_id, operation))
            }
            _ => Ok(lhs),
        }
    }

    fn new_type(
        &mut self,
        type_declaration: NodeID<TypeDeclaration>,
        variable: NodeID<Variable>,
    ) -> Result<TypeDeclaration> {
        let expr_id = self.ast.new_node(variable);
        let expr = Statement::FunctionDeclaration(FunctionDeclaration {
            arguments: vec![],
            returns: NodeType::Custom(type_declaration),
        });
        let expr = self.ast.push(expr_id, expr);

        match self.next_token()?.tp {
            TokenType::LeftCurlyBrace => match self.next_token()?.tp {
                TokenType::RightCurlyBrace => {}
                _ => unimplemented!(),
            },
            _ => unimplemented!(),
        }

        Ok(TypeDeclaration::new(variable, expr))
    }

    fn statement_named(&mut self, name: &str, parent_id: NodeID) -> Result<NodeID<Statement>> {
        let next_token = self.next_token()?;
        let id = match next_token.tp {
            TokenType::VariableDeclaration => {
                let statement_id = self.ast.new_node(parent_id);

                let variable_id = self.ast.new_node(statement_id);
                let variable = Variable::new(name.to_string());
                let variable = self.ast.push(variable_id, variable);

                let value = self.expression(variable)?;
                self.expect_end_statement()?;

                let statement = VariableDeclaration::new(variable, value);
                self.ast.push(statement_id, statement).into()
            }
            TokenType::Assignment => {
                let statement_id = self.ast.new_node(parent_id);

                let value = self.expression(statement_id)?;
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
                let variable_id = self.ast.new_node(statement_id);
                let variable = Variable::new(name.to_string());
                let variable = self.ast.push(variable_id, variable);

                let statement = self.new_type(statement_id, variable)?;
                self.ast.push(statement_id, statement).into()
            }
            TokenType::Dot => {
                let statement_id = self.ast.new_node(parent_id);

                let field = match self.next_token()?.tp {
                    TokenType::Name(path) => path.to_string(),
                    _ => unimplemented!(),
                };

                let variable_id = self.ast.new_node(statement_id);
                let variable = Variable::new(field);
                let variable = self.ast.push(variable_id, variable);

                match self.next_token()?.tp {
                    TokenType::ConstDeclaration => {}
                    _ => unimplemented!(),
                };

                let value = self.expression(statement_id)?;
                self.expect_end_statement()?;

                let statement = StaticAssignment {
                    assign_to: name.to_string().into(),
                    variable,
                    value,
                    is_associated_field: false,
                };
                self.ast.push(statement_id, statement).into()
            }
            tt => unimplemented!("{:?}", tt),
        };
        Ok(id)
    }
}
