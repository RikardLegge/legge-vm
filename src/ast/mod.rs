mod ast;
mod checker;
mod linker;
mod parser;
mod typer;

use std::result;

use crate::runtime::Runtime;
use crate::token::Token;
pub use ast::{
    Ast, Node, NodeBody, NodeID, NodeReferenceType, NodeType, NodeTypeSource, NodeValue,
    UnlinkedNodeBody,
};

pub type Result<N = NodeID> = result::Result<N, Error>;

#[derive(Debug)]
pub struct Error {
    details: String,
}

impl Error {
    pub fn new(details: &str) -> Self {
        let details = format!("Ast Error: {}", details);
        panic!(details);
        // Error { details }
    }
}

pub fn from_tokens<I>(iter: I, runtime: &Runtime) -> Result<Ast>
where
    I: Iterator<Item = Token>,
{
    let mut ast = parser::ast_from_tokens(iter)?;
    linker::link(&mut ast, runtime)?;
    typer::infer_types(&mut ast)?;
    checker::check_types(&ast)?;
    Ok(ast)
}
