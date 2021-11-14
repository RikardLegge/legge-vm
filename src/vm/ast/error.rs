use colored::*;
use std::cmp::{max, min};

use crate::vm::ast::{Ast, IsInvalid, NodeID};
use crate::vm::token::Token;
use std::collections::HashSet;
use std::fmt::Debug;

#[derive(Debug, Clone)]
pub struct ErrPart {
    details: String,
    nodes: Vec<NodeID>,
}

impl ErrPart {
    pub fn new(details: String, nodes: Vec<NodeID>) -> Self {
        ErrPart { details, nodes }
    }
}

#[derive(Debug)]
pub struct Err {
    details: String,
    parts: Vec<ErrPart>,
}

impl ToString for Err {
    fn to_string(&self) -> String {
        self.details.to_string()
    }
}

impl Err {
    pub fn into_err<T>(self, asts: Ast<T>) -> crate::Err
    where
        T: IsInvalid,
    {
        crate::Err::new(format!(
            "\nAst Error: {}\n{}\n",
            self.details,
            self.print_line(&asts)
        ))
    }

    pub fn new(details: String, parts: Vec<ErrPart>) -> Self {
        Self { details, parts }
    }

    pub fn single(details: &str, row_details: &str, nodes: Vec<NodeID>) -> Self {
        Self::new(
            details.into(),
            vec![ErrPart::new(row_details.into(), nodes)],
        )
    }
    fn print_line<T>(&self, asts: &Ast<T>) -> String
    where
        T: Debug,
    {
        let parts = &self.parts;
        if parts.is_empty() {
            return "".into();
        }
        let mut token_parts = parts
            .iter()
            .filter_map(|part| {
                let mut tokens = Vec::new();
                let mut ids = HashSet::new();
                for &node_id in &part.nodes {
                    let ast = &asts.read_ast(node_id.ast());
                    let mut root = ast.get_node(node_id);
                    let mut curr = ast.get_node(node_id);
                    loop {
                        if let Some(t) = root.tokens.first() {
                            let line = t.source.line;
                            let own = curr.id() == root.id();
                            // Inefficient but it works since the parent node might not yet have a reference to it's children.
                            for token in curr.tokens.iter().chain(curr.child_tokens(ast).iter()) {
                                if ids.contains(&token.id) {
                                    continue;
                                }
                                let line_diff = token.source.line as isize - line as isize;
                                if line_diff.abs() > 3 || token.source.line > line {
                                    continue;
                                }
                                ids.insert(token.id);
                                tokens.push((own, token.clone()));
                            }
                            let first_token = curr.tokens.first();
                            let last_token = curr.tokens.last();
                            if let (Some(first), Some(last)) = (first_token, last_token) {
                                if first.source.line != line && last.source.line != line {
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
                    tokens.sort_by(|(_, t1), (_, t2)| t1.source.start.cmp(&t2.source.start));
                    tokens.sort_by(|(_, t1), (_, t2)| t1.source.line.cmp(&t2.source.line));
                    Some((tokens, part))
                }
            })
            .collect::<Vec<(Vec<(bool, Token)>, &ErrPart)>>();
        token_parts.sort_by(|(l, _), (r, _)| l[0].1.source.line.cmp(&r[0].1.source.line));
        token_parts.sort_by(|(_, l), (_, r)| l.nodes[0].ast().cmp(&r.nodes[0].ast()));

        let mut builder = vec![];

        if token_parts.len() == 0 {
            return "Missing tokens to render...".to_string();
        }

        let mut curr_ast = token_parts[0].1.nodes[0].ast();
        let file_name = &asts.read_ast(curr_ast).path.as_ref().as_ref().join("/");
        builder.push(format!("\n"));
        builder.push(format!(" {}.bc \n", file_name));
        builder.push(format!("{}\n", "‾".repeat(file_name.len() + 2)));

        for (tokens, part) in token_parts {
            let i_ast = part.nodes[0].ast();
            if curr_ast != i_ast {
                curr_ast = i_ast;
                let file_name = &asts.read_ast(curr_ast).path.as_ref().as_ref().join("/");
                builder.push(format!(" {} \n", file_name));
                builder.push(format!("{}\n", "‾".repeat(file_name.len() + 2)));
            }
            let mut line = tokens[0].1.source.line;
            let mut end = 0;
            builder.push(format!("{:>4} | ", line));

            let mut underline_start = usize::MAX;
            let mut underline_end = usize::MIN;
            for (own, t) in tokens {
                let mut line_offset = t.source.line as isize - line as isize;
                while line_offset > 0 {
                    end = 0;
                    line_offset -= 1;
                    builder.push(format!("\n{:>4} | ", t.source.line as isize - line_offset));
                }
                let char_offset = t.source.start - end;
                if char_offset > 0 {
                    builder.push(" ".repeat(char_offset));
                }
                let mut token_str = format!("{}", t.tp);
                if own {
                    underline_start = min(underline_start, t.source.start);
                    underline_end = max(underline_end, t.source.end);
                    token_str = token_str.red().to_string()
                }
                builder.push(token_str);
                line = t.source.line;
                end = t.source.end;
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
