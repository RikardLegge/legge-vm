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
pub use debug::Timing;
use std::sync::Arc;

#[allow(dead_code)]
#[derive(Debug, PartialOrd, PartialEq, Copy, Clone)]
pub enum LogLevel {
    LogNone = 0,
    LogDebug = 1,
    LogTiming = 2,
    LogEval = 3,
}

pub fn compile(
    timing: &mut Timing,
    runtime: &runtime::Runtime,
    log_level: LogLevel,
    path: Path,
    code: String,
) -> Option<(bytecode::Bytecode, ValidAstCollection)> {
    let tokio_runtime = tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .build()
        .unwrap();

    let result = ast::from_entrypoint(path, code, &runtime, &tokio_runtime);
    let (asts, ast_timing) = match result {
        Ok((asts, ast_timing)) => (asts, ast_timing),
        Err((asts, e)) => {
            println!("\nAst Error: {}\n{}\n", e.details, e.print_line(&asts));
            return None;
        }
    };
    if log_level >= LogLevel::LogEval {
        println!("{:?}", asts);
    }
    timing.ast = ast_timing;

    let start = debug::start_timer();
    let asts = Arc::new(asts);
    let bytecode = bytecode::from_ast(asts.clone(), &tokio_runtime);
    timing.bytecode = debug::stop_timer(start);
    if log_level >= LogLevel::LogEval {
        println!("{:?}", bytecode);
    }
    let asts = Arc::try_unwrap(asts).unwrap();
    return Some((bytecode, asts));
}

pub fn run_code<F>(path: Path, code: String, log_level: LogLevel, interrupt: F) -> Option<()>
where
    F: Fn(bytecode::Value),
{
    let runtime = runtime::std();
    let mut timing = debug::Timing::default();
    let compiled = compile(&mut timing, &runtime, log_level, path, code);

    let (bytecode, asts) = match compiled {
        Some(v) => v,
        None => return None,
    };

    let mut interpreter = interpreter::Interpreter::new(&runtime);
    interpreter.log_level = log_level;
    interpreter.interrupt = &interrupt;

    let get_line = |node_id: NodeID| match asts.get_node(node_id).tokens.first() {
        Some(token) => token.line,
        None => 0,
    };
    interpreter.get_line = &get_line;

    let start = debug::start_timer();
    timing.instructions = interpreter.run(&bytecode);
    timing.interpreter = debug::stop_timer(start);
    timing.avg_instruction = timing.interpreter / timing.instructions as u32;

    if log_level >= LogLevel::LogTiming {
        timing.line_count = asts.iter().map(|ast| ast.line_count).sum();
        timing.file_count = asts.iter().count();
        dbg!(&timing);
        let mil_lines = timing.line_count as f64 / 1_000_000.0;
        let per_mil = format!("{:.2}s/M rows", timing.total().as_secs_f64() / mil_lines);
        dbg!(per_mil);
    }
    // Prevent de-allocation of the ast, which is slow and not needed for the CLI case.
    let a = Box::new((bytecode, asts));
    Box::leak(a);
    return Some(());
}
