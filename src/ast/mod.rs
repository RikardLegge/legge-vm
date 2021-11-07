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
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use tokio::io::AsyncReadExt;

use crate::runtime::Runtime;
use crate::token::Token;
use crate::{debug, token};

use ast::StateTypesChecked;
pub use ast::{Any, Linked, TypesChecked, TypesInferred};
pub use ast::{
    Ast, AstCollection, AstID, Node, NodeID, NodeReferenceType, NodeType, NodeTypeSource, NodeValue,
};

pub type ValidAstCollection = AstCollection<StateTypesChecked>;

pub type Result<N = NodeID> = result::Result<N, Err>;

#[derive(Debug)]
pub struct Err {
    pub details: String,
    pub parts: Vec<ErrPart>,
}

impl Err {
    fn new(details: String, parts: Vec<ErrPart>) -> Self {
        Self { details, parts }
    }

    pub fn single(details: &str, row_details: &str, nodes: Vec<NodeID>) -> Self {
        Self::new(
            details.into(),
            vec![ErrPart::new(row_details.into(), nodes)],
        )
    }
}

pub type PathKey = Vec<String>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Path(Vec<String>);

impl Path {
    pub fn new(path: Vec<String>) -> Self {
        Self(path)
    }

    pub fn file(&self) -> String {
        format!("{}.bc", self.0[0])
    }

    pub fn key(self) -> PathKey {
        self.0
    }
}

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

impl Err {
    pub fn print_line<T>(&self, asts: &AstCollection<T>) -> String
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
                    let ast = &asts.get(node_id.ast()).read().unwrap();
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
        token_parts.sort_by(|(_, l), (_, r)| l.nodes[0].ast().cmp(&r.nodes[0].ast()));

        let mut builder = vec![];

        if token_parts.len() == 0 {
            return "Missing tokens to render...".to_string();
        }

        let mut curr_ast = token_parts[0].1.nodes[0].ast();
        let file_name = &asts.get(curr_ast).read().unwrap().file_name;
        builder.push(format!("\n"));
        builder.push(format!(" {} \n", file_name));
        builder.push(format!("{}\n", "‾".repeat(file_name.len() + 2)));

        for (tokens, part) in token_parts {
            let i_ast = part.nodes[0].ast();
            if curr_ast != i_ast {
                curr_ast = i_ast;
                let file_name = &asts.get(curr_ast).read().unwrap().file_name;
                builder.push(format!(" {} \n", file_name));
                builder.push(format!("{}\n", "‾".repeat(file_name.len() + 2)));
            }
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

#[derive(Debug)]
enum ToAstTaskState {
    New(Path),
    ToTokens(Path, String),
    ToAst(Path, Vec<Token>),
    Done(Option<(Path, result::Result<Ast, (Ast, Err)>)>),
}

fn to_asts(
    path: Path,
    file: String,
    runtime: &tokio::runtime::Runtime,
) -> result::Result<AstCollection, (AstCollection, Err)> {
    runtime.block_on(async {
        let (tx, mut rx) = tokio::sync::mpsc::unbounded_channel();
        let mut processed_paths = HashSet::new();

        tx.send(ToAstTaskState::ToTokens(path.clone(), file))
            .unwrap();
        processed_paths.insert(path);
        let active_tasks = Arc::new(AtomicUsize::new(1));

        let mut asts = AstCollection::new();
        while let Some(task) = rx.recv().await {
            match task {
                ToAstTaskState::New(path) => {
                    if !processed_paths.contains(&path) {
                        processed_paths.insert(path.clone());
                        let tx = tx.clone();
                        tokio::task::spawn(async move {
                            if let Ok(mut file) = tokio::fs::File::open(path.file()).await {
                                let mut code = String::new();
                                file.read_to_string(&mut code)
                                    .await
                                    .expect("something went wrong reading file");

                                tx.send(ToAstTaskState::ToTokens(path, code)).unwrap();
                            } else {
                                tx.send(ToAstTaskState::Done(None)).unwrap();
                            }
                        });
                    } else {
                        tx.send(ToAstTaskState::Done(None)).unwrap();
                    }
                }
                ToAstTaskState::ToTokens(path, code) => {
                    let tx = tx.clone();
                    let active_tasks = active_tasks.clone();
                    tokio::task::spawn_blocking(move || {
                        let import_tx = tx.clone();
                        let size = Some(code.len() / 2);
                        let tokens =
                            token::from_chars(code.chars(), size, move |import: Vec<String>| {
                                if let [module, rest @ .., _] = &import[..] {
                                    if module == "local" && rest.len() >= 1 {
                                        let path = Path::new(rest.into());
                                        active_tasks.fetch_add(1, Ordering::AcqRel);
                                        import_tx.send(ToAstTaskState::New(path)).unwrap();
                                    }
                                }
                            });
                        tx.send(ToAstTaskState::ToAst(path, tokens)).unwrap();
                    });
                }
                ToAstTaskState::ToAst(path, tokens) => {
                    let ast_id = asts.reserve();

                    let tx = tx.clone();
                    tokio::task::spawn_blocking(move || {
                        let last_token = tokens.last().cloned();
                        let size_hint = tokens.len();
                        let mut ast = parser::ast_from_tokens(
                            path.file(),
                            ast_id,
                            tokens.into_iter(),
                            size_hint,
                        );
                        if let Ok(ast) = &mut ast {
                            ast.line_count = last_token.map(|t| t.line).unwrap_or(0);
                        }
                        tx.send(ToAstTaskState::Done(Some((path, ast)))).unwrap();
                    });
                }
                ToAstTaskState::Done(result) => {
                    active_tasks.fetch_sub(1, Ordering::AcqRel);
                    if let Some((path, res)) = result {
                        match res {
                            Ok(ast) => asts.add(path, ast),
                            Err((ast, err)) => {
                                asts.add(path, ast);
                                return Err((asts, err));
                            }
                        }
                    }
                }
            }
            let active_tasks = active_tasks.load(Ordering::SeqCst);
            if active_tasks == 0 {
                break;
            }
        }
        Ok(asts)
    })
}

pub fn from_entrypoint(
    path: Path,
    code: String,
    runtime: &Runtime,
    tokio_runtime: &tokio::runtime::Runtime,
) -> result::Result<(AstCollection<StateTypesChecked>, debug::AstTiming), (AstCollection, Err)> {
    let mut timing = debug::AstTiming::default();
    let start = debug::start_timer();

    let asts = match to_asts(path, code, tokio_runtime) {
        Ok(asts) => asts,
        Err((asts, err)) => return Err((asts, err)),
    };
    timing.build_ast = debug::stop_timer(start);

    let start = debug::start_timer();
    let asts = match linker::link(asts, runtime, tokio_runtime) {
        Ok(ast) => ast,
        Err((asts, err)) => {
            // println!("{:?}", asts);
            Err((asts.guarantee_state(), err))?
        }
    };
    timing.linker = debug::stop_timer(start);

    let start = debug::start_timer();
    let asts = match typer::infer_types(asts, runtime, tokio_runtime) {
        Ok(asts) => asts,
        Err((asts, err)) => {
            // println!("{:?}", asts);
            Err((asts.guarantee_state(), err))?
        }
    };
    timing.type_inference = debug::stop_timer(start);

    let start = debug::start_timer();
    let asts = match checker::check_types(asts, tokio_runtime) {
        Ok(asts) => asts,
        Err((asts, err)) => {
            // println!("{:?}", asts);
            Err((asts.guarantee_state(), err))?
        }
    };
    timing.type_checker = debug::stop_timer(start);

    let asts = if false {
        let start = debug::start_timer();
        let asts = match treeshaker::treeshake(asts) {
            Ok(asts) => asts,
            Err((asts, err)) => {
                // println!("{:?}", asts);
                Err((asts.guarantee_state(), err))?
            }
        };
        timing.treeshaker = debug::stop_timer(start);
        asts
    } else {
        asts
    };

    Ok((asts, timing))
}
