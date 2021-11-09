use std::time::{Duration, SystemTime};

pub fn start_timer() -> SystemTime {
    SystemTime::now()
}

pub fn stop_timer(start: SystemTime) -> Duration {
    SystemTime::now().duration_since(start).unwrap()
}
