mod ast;
mod linker;
mod parser;
mod typer;

use std::result;

use crate::token::Token;
pub use ast::{Ast, Node, NodeBody, NodeID, NodeType, NodeValue, UnlinkedNodeBody};

pub type Result<N = NodeID> = result::Result<N, Error>;

#[derive(Debug)]
pub struct Error {
    details: String,
}

impl Error {
    fn new(details: &str) -> Self {
        let details = format!("Ast Error: {}", details);
        panic!(details);
        // Error { details }
    }
}

pub fn from_tokens<I>(iter: I) -> Result<Ast>
where
    I: Iterator<Item = Token>,
{
    let mut ast = parser::ast_from_tokens(iter)?;
    linker::link(&mut ast)?;
    typer::infer_types(&mut ast)?;
    Ok(ast)
}
