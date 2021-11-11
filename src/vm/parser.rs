use crate::vm::transform;
use crate::{vm, LogLevel, Path};
use std::sync::Arc;

pub struct Parser<'a> {
    vm_runtime: &'a vm::Runtime,
    tokio_runtime: &'a tokio::runtime::Runtime,
    file_store: &'a dyn vm::FileStore,
    log_level: LogLevel,
}

impl<'a> Parser<'a> {
    pub fn new(
        tokio_runtime: &'a tokio::runtime::Runtime,
        vm_runtime: &'a vm::Runtime,
        file_store: &'a dyn vm::FileStore,
        log_level: LogLevel,
    ) -> Self {
        Self {
            vm_runtime,
            tokio_runtime,
            file_store,
            log_level,
        }
    }

    pub fn parse(&self, path: Path) -> crate::Result<vm::Ast> {
        let tokio = self.tokio_runtime;
        let std = Arc::new(self.vm_runtime.definitions.clone());
        let file_store = self.file_store;
        let mut builder = transform::Builder::new();
        if self.log_level >= LogLevel::LogTiming {
            builder = builder.after_each(&|name, time| println!("{}: {:?}", name, time))
        }
        let ast = builder
            .transform(&transform::Main::new(tokio, file_store, path))?
            .transform(&transform::Link::new(tokio, std.clone()))?
            .transform(&transform::InferTypes::new(tokio, std.clone()))?
            .transform(&transform::CheckTypes::new(tokio))?
            .transform(&transform::TreeShake::new())?
            .build();
        if self.log_level >= LogLevel::LogEval {
            println!("{:?}", ast);
        }
        Ok(ast)
    }
}
