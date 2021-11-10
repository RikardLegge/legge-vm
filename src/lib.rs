#![feature(trait_alias)]
#![deny(unsafe_op_in_unsafe_fn)]

pub mod testing;
pub mod vm;

use std::fmt::{Display, Formatter};
use std::result;

pub type Path = Vec1<String>;
pub type SubPath<'a> = Vec1Ref<'a, String>;

pub struct Vec1Ref<'a, T>(&'a [T]);

impl<'a, T> Vec1Ref<'a, T> {
    pub fn try_new(rest: &'a [T]) -> Option<Vec1Ref<'a, T>> {
        if rest.len() > 0 {
            Some(Self(rest))
        } else {
            None
        }
    }

    pub fn first(&self) -> &T {
        self.0.first().unwrap()
    }

    pub fn not_first(&self) -> Option<Self> {
        if self.0.len() > 1 {
            Some(Self(&self.0[1..]))
        } else {
            None
        }
    }

    pub fn as_ref(&self) -> &[T] {
        &self.0
    }
}

impl<'a, T> Vec1Ref<'a, T>
where
    T: Clone,
{
    pub fn to_owned(&self) -> Vec1<T> {
        Vec1::try_new(self.0.into()).unwrap()
    }
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct Vec1<T>(Vec<T>);

impl<T> Vec1<T> {
    pub fn single(head: T) -> Self {
        Self::try_new(vec![head]).unwrap()
    }

    pub fn try_new(rest: Vec<T>) -> Option<Self> {
        if rest.len() > 0 {
            Some(Self(rest))
        } else {
            None
        }
    }

    pub fn first(&self) -> &T {
        &self.0[0]
    }

    pub fn last(&self) -> &T {
        &self.0[self.0.len() - 1]
    }

    pub fn not_first(&self) -> Option<Vec1Ref<T>> {
        if self.0.len() > 1 {
            Some(Vec1Ref(&self.0[1..]))
        } else {
            None
        }
    }

    pub fn as_ref(&self) -> Vec1Ref<T> {
        Vec1Ref(&self.0[..])
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

pub fn run_code<F>(
    file_store: impl vm::FileStore,
    path: Path,
    log_level: LogLevel,
    leak_ast: bool,
    interrupt: F,
) -> crate::Result<()>
where
    F: Fn(vm::Value),
{
    let vm_runtime = vm::Runtime::default();
    let tokio_runtime = tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .build()
        .unwrap();

    let start = vm::start_timer();
    let compiler = vm::Compiler::new(&tokio_runtime, &vm_runtime, &file_store, log_level);
    let bytecode = compiler.compile(path, leak_ast)?;
    vm::time("Total compile time", start, log_level);

    let mut interpreter = vm::Interpreter::new(&vm_runtime, log_level, &interrupt);
    interpreter.run(&bytecode);

    if leak_ast {
        Box::leak(Box::new(bytecode));
    }

    return Ok(());
}
