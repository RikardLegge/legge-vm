#![feature(trait_alias)]
#![deny(unsafe_op_in_unsafe_fn)]

pub mod testing;
pub mod vm;

use std::fmt::{Display, Formatter};
use std::result;

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

#[allow(dead_code)]
#[derive(Debug, PartialOrd, PartialEq, Copy, Clone)]
pub enum LogLevel {
    LogNone = 0,
    LogDebug = 1,
    LogTiming = 2,
    LogEval = 3,
}

pub type Result<N> = result::Result<N, Err>;

#[derive(Debug)]
pub struct Err {
    details: String,
}

impl Display for Err {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.details)
    }
}

impl Err {
    pub fn new(details: impl Into<String>) -> Self {
        Self {
            details: details.into(),
        }
    }
}

pub fn run_code<F>(path: Path, code: String, log_level: LogLevel, interrupt: F) -> crate::Result<()>
where
    F: Fn(vm::Value),
{
    let vm_runtime = vm::Runtime::default();
    let tokio_runtime = tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .build()
        .unwrap();

    let start = vm::start_timer();
    let compiler = vm::Compiler::new(&tokio_runtime, &vm_runtime, log_level);
    let bytecode = compiler.compile(path, code)?;
    vm::time("Total compile time", start, log_level);

    let mut interpreter = vm::Interpreter::new(&vm_runtime, log_level, &interrupt);
    interpreter.run(&bytecode);

    return Ok(());
}
