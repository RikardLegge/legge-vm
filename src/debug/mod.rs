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
    pub type_inference: Duration,
    pub type_checker: Duration,
}
