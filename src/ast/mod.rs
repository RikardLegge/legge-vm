mod ast;
mod checker;
mod linker;
mod parser;
mod typer;

use std::result;

use crate::debug;
use crate::runtime::Runtime;
use crate::token::Token;
pub use ast::{
    Ast, Node, NodeBody, NodeID, NodeReferenceType, NodeType, NodeTypeSource, NodeValue,
    UnlinkedNodeBody,
};

pub type Result<N = NodeID> = result::Result<N, Err>;

#[derive(Debug)]
pub struct Err {
    details: String,
}

impl Err {
    pub fn new(ast: &Ast, details: &str, row_details: &str, nodes: Vec<NodeID>) -> Self {
        let node_info: Vec<String> = nodes
            .into_iter()
            .map(|n| ast.get_node(n).print_line(ast, row_details))
            .collect();
        let details = format!("\nAst Error: {}\n{}\n", details, node_info.join("\n\n"));
        panic!(details);
        // Err { details }
    }
}

pub fn from_tokens<I>(iter: I, runtime: &Runtime) -> Result<(Ast, debug::AstTiming)>
where
    I: Iterator<Item = Token>,
{
    let mut timing = debug::AstTiming::default();
    let start = debug::start_timer();
    let mut ast = parser::ast_from_tokens(iter)?;
    timing.from_tokens = debug::stop_timer(start);

    let start = debug::start_timer();
    linker::link(&mut ast, runtime)?;
    timing.linker = debug::stop_timer(start);

    let start = debug::start_timer();
    typer::infer_types(&mut ast, runtime)?;
    timing.type_inference = debug::stop_timer(start);

    let start = debug::start_timer();
    checker::check_types(&ast)?;
    timing.type_checker = debug::stop_timer(start);

    Ok((ast, timing))
}
