use crate::{vm, LogLevel, Path};

pub struct Compiler<'a> {
    vm_runtime: &'a vm::Runtime,
    tokio_runtime: &'a tokio::runtime::Runtime,
    file_store: &'a dyn vm::FileStore,
    log_level: LogLevel,
}

impl<'a> Compiler<'a> {
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

    pub fn compile(&self, path: Path, leak_ast: bool) -> crate::Result<vm::Bytecode> {
        let parser = vm::Parser::new(
            self.tokio_runtime,
            self.vm_runtime,
            self.file_store,
            self.log_level,
        );
        let asts = parser.parse(path)?;

        let generator = vm::BytecodeGenerator::new(&self.tokio_runtime, self.log_level);
        let bytecode = generator.generate(asts, leak_ast)?;

        return Ok(bytecode);
    }
}
