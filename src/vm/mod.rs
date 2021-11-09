mod ast;
mod bytecode;
mod compiler;
mod interpreter;
mod parser;
mod runtime;
mod token;

type Ast = ast::Ast<ast::TypesChecked>;

pub use bytecode::{Bytecode, OPCode, Value};

pub use compiler::Compiler;

pub use interpreter::{Interpreter, InterpreterState};

pub use runtime::Runtime;

pub use parser::Parser;

pub use bytecode::BytecodeGenerator;

use crate::LogLevel;
use std::time::{Duration, SystemTime};

pub fn start_timer() -> SystemTime {
    SystemTime::now()
}

pub fn stop_timer(start: SystemTime) -> Duration {
    SystemTime::now().duration_since(start).unwrap()
}

pub fn time(msg: &str, start: SystemTime, log_level: LogLevel) -> SystemTime {
    if log_level >= LogLevel::LogTiming {
        let time = stop_timer(start);
        println!("{}: {:?}", msg, time);
    }
    start_timer()
}
