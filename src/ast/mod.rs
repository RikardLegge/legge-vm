mod ast;
mod checker;
mod linker;
pub mod nodebody;
mod parser;
mod treeshaker;
mod typer;

use colored::*;

use std::collections::HashSet;
use std::fmt::Debug;
use std::result;

use crate::debug;
use crate::runtime::Runtime;
use crate::token::Token;

use crate::ast::ast::StateTypesChecked;
pub use crate::ast::ast::{Any, Linked, TypesChecked, TypesInferred};
pub use ast::{Ast, Node, NodeID, NodeReferenceType, NodeType, NodeTypeSource, NodeValue};

pub type ValidAst = Ast<StateTypesChecked>;

pub type Result<N = NodeID> = result::Result<N, Err>;

#[derive(Debug)]
pub struct Err {
    pub details: String,
    pub node_info: Vec<String>,
}

impl Err {
    pub fn new<T: Debug>(
        ast: &Ast<T>,
        details: &str,
        row_details: &str,
        nodes: Vec<NodeID>,
    ) -> Self {
        let node_info = Self::print_line(nodes, ast, row_details);
        // panic!("\nAst Error: {}\n{}\n", details, node_info.join("\n\n"));
        Err {
            details: details.into(),
            node_info: vec![node_info],
        }
    }

    pub fn print_line<T: Debug>(nodes: Vec<NodeID>, ast: &Ast<T>, msg: &str) -> String {
        if nodes.is_empty() {
            return "".into();
        }
        let all_tokens = {
            let mut tokens = Vec::new();
            let mut ids = HashSet::new();
            for node_id in nodes.iter() {
                let mut curr = ast.get_node(*node_id);
                if let Some(token) = curr.tokens.first() {
                    let line = token.line;
                    loop {
                        // Inefficient but it works since the parent node might not yet have a reference to it's children.
                        for token in curr.tokens.iter().chain(curr.child_tokens(ast).iter()) {
                            if ids.contains(&token.id) {
                                continue;
                            }
                            if (token.line as isize - line as isize).abs() > 1 {
                                continue;
                            }
                            ids.insert(token.id);
                            tokens.push(token.clone());
                        }
                        if curr.parent_id.is_none() {
                            break;
                        }
                        let first_token = curr.tokens.first();
                        let last_token = curr.tokens.last();
                        if !first_token.is_none() {
                            if first_token.unwrap().line != line && last_token.unwrap().line != line
                            {
                                break;
                            }
                        }
                        curr = ast.get_node(curr.parent_id.unwrap());
                    }
                } else {
                    continue;
                }
            }
            tokens.sort_by(|t1, t2| t1.start.cmp(&t2.start));
            tokens.sort_by(|t1, t2| t1.line.cmp(&t2.line));
            tokens
        };

        let mut builder = vec![];
        if all_tokens.len() > 0 {
            let mut line = all_tokens[0].line;
            let mut end = 0;
            builder.push(format!("{:>4} | ", line));

            let mut underline = Vec::new();
            let mut do_underline = false;
            for t in all_tokens.iter() {
                let mut line_offset = t.line as isize - line as isize;
                while line_offset > 0 {
                    if do_underline {
                        builder.push(format!(
                            "\n       {} {}",
                            underline.join(""),
                            msg.to_string()
                        ));
                    }

                    end = 0;
                    line_offset -= 1;
                    underline.clear();
                    do_underline = false;

                    builder.push(format!("\n{:>4} | ", t.line as isize - line_offset));
                }
                let char_offset = t.start - end;
                if char_offset > 0 {
                    builder.push(" ".repeat(char_offset));
                    underline.push(" ".repeat(char_offset));
                }
                let mut token_str = format!("{}", t.tp);
                let contains_token = nodes
                    .iter()
                    .filter(|n| ast.get_node(**n).tokens.contains(t))
                    .next()
                    .is_some();
                if contains_token {
                    underline.push("^".repeat(token_str.len()));
                    do_underline = true;
                    token_str = token_str.red().to_string()
                } else {
                    underline.push(" ".repeat(token_str.len()));
                }
                builder.push(token_str);
                line = t.line;
                end = t.end;
            }
            if do_underline {
                builder.push(format!(
                    "\n       {} {}",
                    underline.join(""),
                    msg.to_string()
                ));
            }
        }
        builder.join("")
    }
}

pub fn from_tokens<I>(
    iter: I,
    runtime: &Runtime,
) -> Result<(Ast<StateTypesChecked>, debug::AstTiming)>
where
    I: Iterator<Item = Token>,
{
    let mut timing = debug::AstTiming::default();
    let start = debug::start_timer();
    let ast = parser::ast_from_tokens(iter)?;
    timing.from_tokens = debug::stop_timer(start);

    let start = debug::start_timer();
    let ast = match linker::link(ast, runtime) {
        Ok(ast) => ast,
        Err((ast, err)) => {
            println!("{:?}", ast);
            Err(err)?
        }
    };
    timing.linker = debug::stop_timer(start);

    let start = debug::start_timer();
    let ast = match typer::infer_types(ast, runtime) {
        Ok(ast) => ast,
        Err((ast, err)) => {
            println!("{:?}", ast);
            Err(err)?
        }
    };
    timing.type_inference = debug::stop_timer(start);

    let start = debug::start_timer();
    let ast = match checker::check_types(ast) {
        Ok(ast) => ast,
        Err((ast, err)) => {
            println!("{:?}", ast);
            Err(err)?
        }
    };
    timing.type_checker = debug::stop_timer(start);

    let start = debug::start_timer();
    let ast = match treeshaker::treeshake(ast) {
        Ok(ast) => ast,
        Err((ast, err)) => {
            println!("{:?}", ast);
            Err(err)?
        }
    };
    timing.treeshaker = debug::stop_timer(start);

    Ok((ast, timing))
}
