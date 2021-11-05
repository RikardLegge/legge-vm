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
    let result = ast::from_entrypoint(path, code, &runtime);
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
    let bytecode = bytecode::from_ast(&asts);
    timing.bytecode = debug::stop_timer(start);
    if log_level >= LogLevel::LogEval {
        println!("{:?}", bytecode);
    }
    return Some((bytecode, asts));
}

pub fn run_code<F>(path: Path, code: String, log_level: LogLevel, interrupt: F) -> Option<()>
where
    F: Fn(bytecode::Value),
{
    let runtime = runtime::std();
    let mut timing = debug::Timing::default();
    let compiled = compile(&mut timing, &runtime, log_level, path, code);
    if compiled.is_none() {
        return None;
    }
    let (bytecode, asts) = compiled.unwrap();

    let mut interpreter = interpreter::Interpreter::new(&runtime);
    interpreter.log_level = log_level;
    interpreter.interrupt = &interrupt;

    let get_line = |node_id: NodeID| match asts
        .get(node_id.ast())
        .borrow()
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
