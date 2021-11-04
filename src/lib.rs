#![feature(trait_alias)]
#![deny(unsafe_op_in_unsafe_fn)]

pub mod ast;
pub mod bytecode;
mod debug;
pub mod interpreter;
pub mod runtime;
pub mod testing;
pub mod token;

use crate::ast::{NodeID, Path, ValidAstCollection};
use crate::token::Token;
pub use debug::Timing;
use std::collections::HashMap;
use std::fs::File;
use std::io::Read;

#[allow(dead_code)]
#[derive(Debug, PartialOrd, PartialEq, Copy, Clone)]
pub enum LogLevel {
    LogNone = 0,
    LogDebug = 1,
    LogTiming = 2,
    LogEval = 3,
}

fn tokenize_recurse(code: String) -> HashMap<Path, Vec<Token>> {
    let mut queue = Vec::new();
    let mut all_tokens = HashMap::new();

    let (tokens, mut imports) = token::from_chars(code.chars(), Some(code.len() / 5));
    queue.append(&mut imports);
    all_tokens.insert(Path::empty(), tokens);

    while let Some(path) = queue.pop() {
        if let [module, path @ .., _] = &path[..] {
            if module == "local" && path.len() >= 1 {
                let filename = &path[0];
                let path = Path::new(path.into());
                if !all_tokens.contains_key(&path) {
                    let filepath = [filename, "bc"].join(".");
                    if let Ok(mut file) = File::open(filepath) {
                        let mut code = String::new();
                        file.read_to_string(&mut code)
                            .expect("something went wrong reading file");

                        let (tokens, mut imports) =
                            token::from_chars(code.chars(), Some(code.len() / 5));
                        queue.append(&mut imports);
                        all_tokens.insert(path, tokens);
                    }
                }
            }
        }
    }
    all_tokens
}

pub fn compile(
    timing: &mut Timing,
    runtime: &runtime::Runtime,
    log_level: LogLevel,
    code: String,
) -> Option<(bytecode::Bytecode, ValidAstCollection)> {
    let start = debug::start_timer();
    let tokens = tokenize_recurse(code);
    timing.token = debug::stop_timer(start);

    let result = ast::from_tokens(tokens, &runtime);
    let (asts, ast_timing) = match result {
        Ok((asts, ast_timing)) => (asts, ast_timing),
        Err(e) => {
            println!("Ast Error: {}\n{}\n", e.details, e.node_info);
            return None;
        }
    };
    if log_level >= LogLevel::LogEval {
        println!("{:?}", asts);
    }
    timing.ast = ast_timing;

    let ast = asts.iter().next().unwrap();
    let start = debug::start_timer();
    let bytecode = bytecode::from_ast(ast);
    timing.bytecode = debug::stop_timer(start);
    if log_level >= LogLevel::LogEval {
        println!("{:?}", bytecode);
    }
    return Some((bytecode, asts));
}

pub fn run_code<F>(code: String, log_level: LogLevel, interrupt: F) -> Option<()>
where
    F: Fn(bytecode::Value),
{
    let runtime = runtime::std();
    let mut timing = debug::Timing::default();
    let compiled = compile(&mut timing, &runtime, log_level, code);
    if compiled.is_none() {
        return None;
    }
    let (bytecode, asts) = compiled.unwrap();

    let mut interpreter = interpreter::Interpreter::new(&runtime);
    interpreter.log_level = log_level;
    interpreter.interrupt = &interrupt;

    let get_line = |node_id: NodeID| match asts
        .get(node_id.ast())
        .unwrap()
        .get_node(node_id)
        .tokens
        .first()
    {
        Some(token) => token.line,
        None => 0,
    };
    interpreter.get_line = &get_line;

    let start = debug::start_timer();
    timing.instructions = interpreter.run(&bytecode);
    timing.interpreter = debug::stop_timer(start);
    timing.avg_instruction = timing.interpreter / timing.instructions as u32;

    if log_level >= LogLevel::LogTiming {
        dbg!(&timing);
        dbg!(timing.total());
    }
    return Some(());
}
