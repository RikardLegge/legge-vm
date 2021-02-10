pub mod ast;
pub mod bytecode;
mod debug;
pub mod interpreter;
pub mod runtime;
pub mod token;

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
    code: String,
) -> bytecode::Bytecode {
    let start = debug::start_timer();
    let tokens = token::from_chars(code.chars());
    timing.token = debug::stop_timer(start);

    let (ast, ast_timing) = ast::from_tokens(tokens.into_iter(), &runtime).unwrap();
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
    return bytecode;
}

pub fn run_code<F>(code: String, log_level: LogLevel, interrupt: F)
where
    F: Fn(bytecode::Value),
{
    let runtime = runtime::std();
    let mut timing = debug::Timing::default();
    let bytecode = compile(&mut timing, &runtime, log_level, code);

    let mut interpreter = interpreter::Interpreter::new(&runtime);
    interpreter.log_level = log_level;
    interpreter.interrupt = &interrupt;

    let start = debug::start_timer();
    timing.instructions = interpreter.run(&bytecode);
    timing.interpreter = debug::stop_timer(start);
    timing.avg_instruction = timing.interpreter / timing.instructions as u32;

    if log_level >= LogLevel::LogTiming {
        dbg!(timing);
    }
}
