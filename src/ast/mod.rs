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
    pub details: String,
    pub node_info: Vec<String>,
}

impl Err {
    pub fn new(ast: &Ast, details: &str, row_details: &str, nodes: Vec<NodeID>) -> Self {
        let node_info = Self::print_line(nodes, ast, row_details);
        // panic!("\nAst Error: {}\n{}\n", details, node_info.join("\n\n"));
        Err { details: details.into(), node_info: vec![node_info] }
    }

    pub fn print_line(nodes: Vec<NodeID>, ast: &Ast, msg: &str) -> String {
        let mut tokens = Vec::new();
        for node_id in nodes.iter() {
            let node = ast.get_node(*node_id);
            tokens.append(&mut node.tokens.clone());
            for child_id in node.body.children() {
                let child = ast.get_node(*child_id);
                tokens.append(&mut child.tokens.clone());
                tokens.append(&mut child.child_tokens(ast));
            }
        }
        tokens.sort_by(|t1, t2| t1.start.cmp(&t2.start));
        tokens.sort_by(|t1, t2| t1.line.cmp(&t2.line));
        if tokens.len() == 0 {
            return "generated (More details should be added in the future)".into();
        }
        let mut line = tokens[0].line;
        let mut end = 0;
        let mut builder = vec![format!("{:>4} | ", line)];

        let mut underline = Vec::new();
        let mut do_underline = false;
        for t in tokens.iter() {
            let mut line_offset = t.line - line;
            while line_offset > 0 {
                if do_underline {
                    builder.push(format!(
                        "\n       {} {}",
                        underline.join(""),
                        msg.to_string()
                    ));
                }
                builder.push(format!("\n{:>4} | ", t.line));
                end = 0;
                line_offset -= 1;
                underline.clear();
                do_underline = false;
            }
            let char_offset = t.start - end;
            if char_offset > 0 {
                builder.push(" ".repeat(char_offset));
                underline.push(" ".repeat(char_offset));
            }
            let token_str = format!("{:?}", t.tp);
            let contains_token = nodes
                .iter()
                .filter(|n| ast.get_node(**n).tokens.contains(t))
                .next()
                .is_some();
            if contains_token {
                underline.push("^".repeat(token_str.len()));
                do_underline = true;
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
        builder.join("")
    }
}

pub fn from_tokens<I>(iter: I, runtime: &Runtime) -> Result<(Ast, debug::AstTiming)>
    where
        I: Iterator<Item=Token>,
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
