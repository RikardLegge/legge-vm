use std::time::{Duration, SystemTime};

#[derive(Default, Debug)]
pub struct Timing {
    pub token: Duration,
    pub ast: AstTiming,
    pub bytecode: Duration,
    pub interpreter: Duration,
    pub instructions: usize,
    pub avg_instruction: Duration,
}
impl Timing {
    pub fn total(&self) -> Duration {
        self.token + self.bytecode + self.interpreter + self.ast.total()
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
    pub from_tokens: Duration,
    pub linker: Duration,
    pub treeshaker: Duration,
    pub type_inference: Duration,
    pub type_checker: Duration,
}

impl AstTiming {
    pub fn total(&self) -> Duration {
        self.from_tokens + self.linker + self.type_inference + self.type_checker
    }
}
