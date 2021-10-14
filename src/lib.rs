#![feature(box_patterns)]

pub mod ast;
pub mod bytecode;
mod debug;
pub mod interpreter;
pub mod runtime;
pub mod token;
pub mod testing;

pub use debug::Timing;
use crate::ast::Ast;

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
    code: String,
) -> Option<(bytecode::Bytecode, Ast)> {
    let start = debug::start_timer();
    let tokens = token::from_chars(code.chars());
    timing.token = debug::stop_timer(start);

    let result = ast::from_tokens(tokens.into_iter(), &runtime);
    let (ast, ast_timing) = match result {
        Ok((ast, ast_timing)) => (ast, ast_timing),
        Err(e) => {
            println!("Ast Error: {}\n{}\n",e.details, e.node_info.join("\n\n"));
            return None;
        }
    };
    if log_level >= LogLevel::LogTiming {
        println!("{:?}", ast);
    }
    timing.ast = ast_timing;

    let start = debug::start_timer();
    let bytecode = bytecode::from_ast(&ast);
    timing.bytecode = debug::stop_timer(start);
    if log_level >= LogLevel::LogTiming {
        println!("{:?}", bytecode);
    }
    return Some((bytecode, ast));
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
    let (bytecode, ast) = compiled.unwrap();

    let mut interpreter = interpreter::Interpreter::new(&runtime);
    interpreter.log_level = log_level;
    interpreter.interrupt = &interrupt;

    let get_line = |node_id| match ast.get_node(node_id).tokens.first() {
        Some(token) => token.line,
        None => 0
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
