use std::cell::RefCell;
use std::rc::Rc;

use crate::vm::{Value, VirtualFileStore};
use crate::LogLevel;
use crate::{run_code, Path};

pub fn run_test(code: &str, expected_result: Option<Value>) {
    let code = format!("import std.exit;{}", code);
    let path = Path::new(vec!["test".to_string(), "generated".to_string()]);
    let mut store = VirtualFileStore::new();
    store.add(path.clone(), code);
    let result = Rc::new(RefCell::new(None));
    let assign_result = result.clone();
    run_code(store, path, LogLevel::LogNone, false, move |v| {
        *assign_result.borrow_mut() = Some(v);
    })
    .expect("code should compile and run");
    let result = result.borrow();
    assert_eq!(*result, expected_result);
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

pub use bc_test;
pub use bc_test_should_fail;
