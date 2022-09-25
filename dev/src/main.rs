mod ast;
mod ast_builder;
mod linker;
mod node;
mod token;

use crate::ast::Ast;
use crate::node::{
    AstNode, Block, Expression, Node, NodeID, NodeType, Operation, Statement, Value, Variable,
    VariableAssignment, VariableDeclaration,
};
use crate::token::TokenType;
use std::fmt::Debug;

#[derive(Debug)]
pub enum Error {
    EOF,
    InternalError,
    VariableNotFound,
    ExpectedEndStatement,
    TypeNotInferred,
}

pub type Result<T, E = Error> = std::result::Result<T, E>;

fn main() -> Result<()> {
    let tokens = token::from_chars(
        r#"
    A -> type {}
    // A.value :: 2;
     
    a := 1; 
    a = a + 1; 
    b := "😇";
    c := a;
    "#,
    );
    println!(
        "[\n  {}\n]",
        tokens
            .iter()
            .map(|t| format!("{:?}", t))
            .collect::<Vec<String>>()
            .join(",\n  ")
    );

    let ast = ast::from_tokens(tokens);
    println!("{:?}", ast);

    let ast = linker::link_ast(ast?)?;
    println!("{:?}", ast);
    Ok(())
}

#[derive(Debug)]
pub enum State<Unlinked, Linked> {
    Unlinked(Unlinked),
    Linked(Linked),
}
