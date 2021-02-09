use crate::bytecode::{Bytecode, Value};
use interpreter::{InterpLogLevel, Interpreter};

mod ast;
pub mod bytecode;
mod debug;
mod interpreter;
mod runtime;
mod token;

pub fn compile(timing: &mut debug::Timing, logging: bool, code: String) -> Bytecode {
    let runtime = runtime::get();

    let start = debug::start_timer();
    let tokens = token::from_chars(code.chars());
    timing.token = debug::stop_timer(start);

    let (ast, ast_timing) = ast::from_tokens(tokens.into_iter(), &runtime).unwrap();
    if logging {
        println!("{:?}", ast);
    }
    timing.ast = ast_timing;

    let start = debug::start_timer();
    let bytecode = bytecode::from_ast(&ast);
    timing.bytecode = debug::stop_timer(start);
    if logging {
        println!("{:?}", bytecode);
    }
    return bytecode;
}

pub fn run_code<F>(code: String, logging: bool, interrupt: F)
where
    F: Fn(Value),
{
    let mut timing = debug::Timing::default();
    let bytecode = compile(&mut timing, logging, code);
    let runtime = runtime::get();

    let log_level = if logging {
        InterpLogLevel::LogEval
    } else {
        InterpLogLevel::LogNone
    };
    let mut interpreter = Interpreter::new(&runtime);
    interpreter.log_level = log_level;
    interpreter.interrupt = &interrupt;

    let start = debug::start_timer();
    timing.instructions = interpreter.run(&bytecode);
    timing.interpreter = debug::stop_timer(start);
    timing.avg_instruction = timing.interpreter / timing.instructions as u32;

    if logging {
        dbg!(timing);
    }
}
