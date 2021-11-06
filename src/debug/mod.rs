use std::time::{Duration, SystemTime};

#[derive(Default, Debug)]
pub struct Timing {
    pub ast: AstTiming,
    pub bytecode: Duration,
    pub interpreter: Duration,
    pub line_count: usize,
    pub file_count: usize,
    pub instructions: usize,
    pub avg_instruction: Duration,
}
impl Timing {
    pub fn total(&self) -> Duration {
        self.bytecode + self.interpreter + self.ast.total()
    }
}

pub fn start_timer() -> SystemTime {
    SystemTime::now()
}

pub fn stop_timer(start: SystemTime) -> Duration {
    SystemTime::now().duration_since(start).unwrap()
}

#[derive(Default, Debug)]
pub struct AstTiming {
    pub build_ast: Duration,
    pub linker: Duration,
    pub treeshaker: Duration,
    pub type_inference: Duration,
    pub type_checker: Duration,
}

impl AstTiming {
    pub fn total(&self) -> Duration {
        self.build_ast + self.linker + self.treeshaker + self.type_inference + self.type_checker
    }
}
