use crate::vm::transform;
use crate::{vm, LogLevel, Path};

pub struct Parser<'a> {
    vm_runtime: &'a vm::Runtime,
    tokio_runtime: &'a tokio::runtime::Runtime,
    log_level: LogLevel,
}

impl<'a> Parser<'a> {
    pub fn new(
        tokio_runtime: &'a tokio::runtime::Runtime,
        vm_runtime: &'a vm::Runtime,
        log_level: LogLevel,
    ) -> Self {
        Self {
            vm_runtime,
            tokio_runtime,
            log_level,
        }
    }

    pub fn parse(&self, path: Path, code: String) -> crate::Result<vm::Ast> {
        let tokio = &self.tokio_runtime;
        let std = &self.vm_runtime;
        let mut builder = transform::Builder::new();
        if self.log_level >= LogLevel::LogTiming {
            builder = builder.after_each(&|name, time| println!("{}: {:?}", name, time))
        }
        let ast = builder
            .transform(&transform::Main::new(tokio, path, code))?
            .transform(&transform::Link::new(tokio, std))?
            .transform(&transform::InferTypes::new(tokio, std))?
            .transform(&transform::CheckTypes::new(tokio))?
            .transform(&transform::TreeShake::new())?
            .build();
        Ok(ast)
    }
}
