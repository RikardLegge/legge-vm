use crate::node::VariableValue;
use crate::token::Token;
use crate::{Ast, Block, Error, Node, TokenType};
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
            ast: Ast::new(),
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
        while let Some(token) = self.tokens.next() {
            match token.tp {
                TokenType::Name(name) => {
                    let statement = self.statement_named(name, block_id.into())?;
                    children.push(statement);
                    self.expect_end_statement()?;
                }
                _ => unimplemented!(),
            }
        }
        let block = Block::new(children, &self.ast);
        let root = self.ast.push(block_id, block);
        self.ast.root = Some(root);
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

    fn next_token(&mut self) -> Result<Token> {
        self.tokens.next().ok_or(Error::EOF)
    }

    fn expression(&mut self, parent_id: NodeID) -> Result<NodeID<Expression>> {
        let next_token = self.next_token()?;
        let expression: Expression = match next_token.tp {
            TokenType::Int(value, _) => Value::Int(value).into(),
            TokenType::Float(value, _) => Value::Float(value).into(),
            TokenType::String(value) => Value::String(value.to_string()).into(),
            TokenType::Name(value) => {
                Expression::VariableValue(VariableValue::new(State::Unlinked(value.to_string())))
            }
            tp => unimplemented!("{:?}", tp),
        };
        let expression_id = self.ast.new_node(parent_id);
        let lhs = self.ast.push(expression_id, expression);

        let rhs_token = match self.peek_token() {
            Ok(token) => token,
            _ => return Ok(lhs),
        };

        match &rhs_token.tp {
            TokenType::Op(op) => {
                let op = *op;
                self.next_token()?;
                let operation_id = self.ast.new_node(parent_id);

                let rhs = self.expression(operation_id.into())?;
                let lhs_node = self.ast.get_mut(lhs);
                lhs_node.parent_id = Some(operation_id.into());

                let operation = Expression::Operation(Operation::new(op, lhs, rhs));
                Ok(self.ast.push(operation_id, operation))
            }
            _ => Ok(lhs),
        }
    }

    fn statement_named(&mut self, name: &str, parent_id: NodeID) -> Result<NodeID<Statement>> {
        let statement_id = self.ast.new_node(parent_id);
        let next_token = self.next_token()?;
        let statement: Statement = match next_token.tp {
            TokenType::VariableDeclaration => {
                let variable_id = self.ast.new_node(statement_id);
                let variable = Variable::new(name.to_string());
                let variable = self.ast.push(variable_id, variable);

                let value = self.expression(variable.into())?;
                VariableDeclaration::new(variable, value).into()
            }
            TokenType::Assignment => {
                let value = self.expression(statement_id.into())?;
                VariableAssignment::new(name.to_string(), value).into()
            }
            _ => unimplemented!(),
        };
        Ok(self.ast.push(statement_id, statement))
    }
}
