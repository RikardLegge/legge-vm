use crate::vm;
use crate::vm::ast::{AstBranch, IsValid, Valid};
use crate::vm::token;
use crate::vm::token::Token;
use crate::vm::transform::token_to_ast;
use crate::vm::{ast, transform};
use crate::Path;
use std::collections::HashSet;
use std::fmt::Debug;

#[derive(Debug)]
enum ToAstTaskState<T>
where
    T: IsValid,
{
    New(Path, bool),
    ToTokens(Path, String),
    ToAst(Path, Vec<Token>),
    Retry(Path, usize),
    Err(Path, ast::Err, Option<AstBranch<T>>),
    Done(Path, AstBranch<T>),
}

pub struct Main<'a> {
    runtime: &'a tokio::runtime::Runtime,
    file_store: &'a dyn vm::FileStore,
    path: Path,
}

impl<'a> Main<'a> {
    pub fn new(
        runtime: &'a tokio::runtime::Runtime,
        file_store: &'a dyn vm::FileStore,
        path: Path,
    ) -> Self {
        Self {
            runtime,
            file_store,
            path,
        }
    }
}

impl<'a, T> transform::AstTransformation<T, Valid> for Main<'a>
where
    T: IsValid,
{
    fn name(&self) -> String {
        "Read Files".to_string()
    }

    fn transform(&self, mut asts: ast::Ast<T>) -> transform::Result<Valid> {
        self.runtime.block_on(async {
            let (tx, mut rx) = tokio::sync::mpsc::unbounded_channel();
            let mut processed_paths = HashSet::new();

            let task = ToAstTaskState::New(self.path.clone(), false);
            tx.send(task).unwrap();

            let mut n_started = 0;
            let mut open_files = 0;
            let mut n_completed = 0;
            let mut pending_files = Vec::new();
            let mut error = None;
            while let Some(task) = rx.recv().await {
                if error.is_some() {
                    n_completed += 1;
                } else {
                    match task {
                        ToAstTaskState::New(path, is_retry) => {
                            if !processed_paths.contains(&path) || is_retry {
                                processed_paths.insert(path.clone());
                                if !is_retry {
                                    n_started += 1;
                                }

                                let tx = tx.clone();
                                open_files += 1;
                                let file = self.file_store.file(&path);
                                tokio::task::spawn(async move {
                                    match file.read().await {
                                        Ok(code) => {
                                            tx.send(ToAstTaskState::ToTokens(path, code)).unwrap();
                                        }
                                        Err(err) => {
                                            if err.retryable() {
                                                tx.send(ToAstTaskState::Retry(path, open_files))
                                                    .unwrap();
                                            } else {
                                                panic!("{:?}", err);
                                            }
                                        }
                                    };
                                });
                            }
                        }
                        ToAstTaskState::Retry(path, error_count) => {
                            if open_files < error_count {
                                tx.send(ToAstTaskState::New(path, true)).unwrap();
                            } else {
                                pending_files.push(path);
                            }
                            open_files -= 1;
                        }
                        ToAstTaskState::ToTokens(path, code) => {
                            open_files -= 1;
                            if let Some(path) = pending_files.pop() {
                                tx.send(ToAstTaskState::New(path, true)).unwrap();
                            }
                            let tx = tx.clone();
                            tokio::task::spawn_blocking(move || {
                                let size = Some(code.len() / 2);
                                let tokens =
                                    token::from_chars(code.chars(), size, |import: Vec<String>| {
                                        if let [module, rest @ .., _] = &import[..] {
                                            if module == "local" && rest.len() >= 1 {
                                                let path = Path::new(rest.into());
                                                tx.send(ToAstTaskState::New(path, false)).unwrap();
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
                                let ast = token_to_ast::ast_from_tokens(
                                    path.file(),
                                    ast_id,
                                    tokens.into_iter(),
                                    size_hint,
                                );
                                match ast {
                                    Ok(mut ast) => {
                                        ast.line_count = last_token.map(|t| t.line).unwrap_or(0);
                                        tx.send(ToAstTaskState::Done(path, ast)).unwrap();
                                    }
                                    Err((ast, err)) => {
                                        tx.send(ToAstTaskState::Err(path, err, Some(ast))).unwrap();
                                    }
                                }
                            });
                        }
                        ToAstTaskState::Err(path, err, ast) => {
                            n_completed += 1;
                            if let Some(ast) = ast {
                                asts.add(path, ast);
                            }
                            error = Some(err);
                        }
                        ToAstTaskState::Done(path, ast) => {
                            n_completed += 1;
                            asts.add(path, ast);
                        }
                    }
                }
                if n_started == n_completed {
                    asts.debug_info.line_count = asts.iter().map(|ast| ast.line_count).sum();
                    asts.debug_info.file_count = asts.iter().count();

                    return if let Some(err) = error {
                        Err((asts.guarantee_state(), err))
                    } else {
                        Ok(asts.guarantee_state())
                    };
                }
            }
            unreachable!();
        })
    }
}
