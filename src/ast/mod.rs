mod ast;
mod checker;
mod linker;
pub mod nodebody;
mod parser;
mod treeshaker;
mod typer;

use colored::*;
use std::cmp::{max, min};

use std::collections::HashSet;
use std::fmt::Debug;
use std::result;

use crate::debug;
use crate::runtime::Runtime;
use crate::token::Token;

use ast::StateTypesChecked;
pub use ast::{Any, Linked, TypesChecked, TypesInferred};
pub use ast::{Ast, Node, NodeID, NodeReferenceType, NodeType, NodeTypeSource, NodeValue};

pub type ValidAst = Ast<StateTypesChecked>;

pub type Result<N = NodeID> = result::Result<N, Err>;

#[derive(Debug, Clone)]
pub struct ErrPart {
    pub details: String,
    pub nodes: Vec<NodeID>,
}

impl ErrPart {
    pub fn new(details: String, nodes: Vec<NodeID>) -> Self {
        ErrPart { details, nodes }
    }
}

#[derive(Debug)]
pub struct Err {
    pub details: String,
    pub node_info: String,
    pub parts: Vec<ErrPart>,
}

impl Err {
    pub fn from_parts<T>(ast: &Ast<T>, details: String, parts: Vec<ErrPart>) -> Self
    where
        T: Debug,
    {
        let node_info = Self::print_line(ast, &parts);
        Err {
            details,
            node_info,
            parts,
        }
    }

    pub fn print_line<T>(ast: &Ast<T>, parts: &[ErrPart]) -> String
    where
        T: Debug,
    {
        if parts.is_empty() {
            return "".into();
        }
        let mut token_parts = parts
            .iter()
            .filter_map(|part| {
                let mut tokens = Vec::new();
                let mut ids = HashSet::new();
                for &node_id in &part.nodes {
                    let mut root = ast.get_node(node_id);
                    let mut curr = ast.get_node(node_id);
                    loop {
                        if let Some(t) = root.tokens.first() {
                            let line = t.line;
                            let own = curr.id() == root.id();
                            // Inefficient but it works since the parent node might not yet have a reference to it's children.
                            for token in curr.tokens.iter().chain(curr.child_tokens(ast).iter()) {
                                if ids.contains(&token.id) {
                                    continue;
                                }
                                let line_diff = token.line as isize - line as isize;
                                if line_diff.abs() > 3 || token.line > line {
                                    continue;
                                }
                                ids.insert(token.id);
                                tokens.push((own, token.clone()));
                            }
                            let first_token = curr.tokens.first();
                            let last_token = curr.tokens.last();
                            if let (Some(first), Some(last)) = (first_token, last_token) {
                                if first.line != line && last.line != line {
                                    break;
                                }
                            }
                            if let Some(parent_id) = curr.parent_id {
                                curr = ast.get_node(parent_id);
                            } else {
                                break;
                            }
                        } else if let Some(parent_id) = root.parent_id {
                            root = ast.get_node(parent_id);
                            curr = ast.get_node(parent_id);
                        } else {
                            break;
                        }
                    }
                }
                if tokens.is_empty() {
                    None
                } else {
                    tokens.sort_by(|(_, t1), (_, t2)| t1.start.cmp(&t2.start));
                    tokens.sort_by(|(_, t1), (_, t2)| t1.line.cmp(&t2.line));
                    Some((tokens, part))
                }
            })
            .collect::<Vec<(Vec<(bool, Token)>, &ErrPart)>>();
        token_parts.sort_by(|(l, _), (r, _)| l[0].1.line.cmp(&r[0].1.line));

        let mut builder = vec![];
        for (tokens, part) in token_parts {
            let mut line = tokens[0].1.line;
            let mut end = 0;
            builder.push(format!("{:>4} | ", line));

            let mut underline_start = usize::MAX;
            let mut underline_end = usize::MIN;
            for (own, t) in tokens {
                let mut line_offset = t.line as isize - line as isize;
                while line_offset > 0 {
                    end = 0;
                    line_offset -= 1;
                    builder.push(format!("\n{:>4} | ", t.line as isize - line_offset));
                }
                let char_offset = t.start - end;
                if char_offset > 0 {
                    builder.push(" ".repeat(char_offset));
                }
                let mut token_str = format!("{}", t.tp);
                if own {
                    underline_start = min(underline_start, t.start);
                    underline_end = max(underline_end, t.end);
                    token_str = token_str.red().to_string()
                }
                builder.push(token_str);
                line = t.line;
                end = t.end;
            }
            let prefix = " ".repeat(underline_start);
            let underline = "^".repeat(underline_end - underline_start);
            builder.push(format!(
                "\n       {}{} {}\n\n",
                prefix, underline, part.details
            ));
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
