use std::cell::RefCell;
use std::rc::Rc;

use crate::vm::{Value, VirtualFileStore};
use crate::LogLevel;
use crate::{run_code, Path};

fn run(code: &str) -> Option<Value> {
    let code = format!("import std.exit;{}", code);
    let path = Path::single("test".to_string());
    let mut store = VirtualFileStore::new();
    store.add(path.clone(), code);
    let result = Rc::new(RefCell::new(None));
    {
        let assign_result = result.clone();
        run_code(store, path, LogLevel::LogNone, false, move |v| {
            *assign_result.borrow_mut() = Some(v);
        })
        .expect("code should compile and run");
    }
    Rc::try_unwrap(result).expect("").into_inner()
}

pub fn run_test(code: &str, expected_result: Option<Value>) {
    let result = run(code);
    assert_eq!(result, expected_result);
}

pub fn interpret_code(code: &str) -> Value {
    let result = run(code);
    match result {
        Some(v) => v,
        None => Value::Unset,
    }
}

#[macro_export]
macro_rules! bc_test_should_fail {
    ( $name:ident $bc:tt) => {
        #[test]
        #[should_panic]
        fn $name() {
            leggevm::testing::run_test(stringify!($bc), None);
        }
    };
}

#[macro_export]
macro_rules! bc_test {
    ( $name:ident $bc:tt) => {
        #[test]
        fn $name() {
            leggevm::testing::run_test(stringify!($bc), None);
        }
    };
    ( $name:ident $bc:tt == $result:expr ) => {
        #[test]
        fn $name() {
            leggevm::testing::run_test(stringify!($bc), Some($result));
        }
    };
}

#[macro_export]
macro_rules! interpret {
    ( $bc:tt ) => {
        leggevm::testing::interpret_code(stringify!($bc))
    };
}

pub use bc_test;
pub use bc_test_should_fail;
pub use interpret;
