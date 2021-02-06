use std::cell::RefCell;
use std::rc::Rc;

use leggevm::bytecode::Value;
use leggevm::bytecode::Value::*;
use leggevm::run_code;

fn run_test(code: &str, expected_result: Option<Value>) {
    let code = format!("import exit;{}", code);
    let result = Rc::new(RefCell::new(None));
    let assign_result = result.clone();
    run_code(code.into(), false, move |v| {
        *assign_result.borrow_mut() = Some(v);
    });
    let result = result.borrow();
    assert_eq!(*result, expected_result);
}

#[test]
fn test_variable() {
    run_test("a :: 1; exit(a);", Some(Int(1)));
}

#[test]
fn test_add() {
    run_test("a :: 1; b :: 2; c :: a + b; exit(c);", Some(Int(3)));
}

#[test]
fn test_sub() {
    run_test("a :: 1; b :: 2; c :: a - b; exit(c);", Some(Int(-1)));
}

#[test]
fn test_mult() {
    run_test("a :: 2; b :: 3; c :: a * b; exit(c);", Some(Int(6)));
}

#[test]
fn test_div() {
    run_test("a :: 4; b :: 2; c :: a / b; exit(c);", Some(Int(2)));
}

#[test]
fn test_if_true() {
    run_test("if(true) {exit(1);} exit(0);", Some(Int(1)));
}

#[test]
fn test_if_fales() {
    run_test("if(false) {exit(1);} exit(0);", Some(Int(0)));
}

#[test]
fn test_function() {
    run_test(
        "
        double :: fn(i: int) -> int {
            return 2 * i;
        }
        exit(double(2));
        ",
        Some(Int(4)),
    );
}

#[test]
fn test_pass_function() {
    run_test(
        "
        double :: fn(i: int) -> int {
            return 2 * i;
        }
        exec :: fn(num: int, map: Fn(int) -> int) -> int {
            return map(num);
        }
        exit(exec(2, double));
        ",
        Some(Int(4)),
    );
}

#[test]
fn test_comments_after_return() {
    run_test(
        "
        exec :: fn() -> int {
            return 4; 
            // a comment
        }
        exit(exec());",
        Some(Int(4)),
    );
}

#[test]
fn test_return_function() {
    run_test(
        "
        get :: fn() -> Fn(int) -> int {
            return fn(i: int) -> int { return 2 * i; };
        }
        func :: get();
        exit(func(2));
        ",
        Some(Int(4)),
    );
}

#[test]
fn test_var_assign() {
    run_test("a := 1; a = 2;", None);
}

#[test]
fn test_loop() {
    run_test(
        "i := 0; loop { if(i == 100) {break;} i = i + 1;} exit(i);",
        Some(Int(100)),
    );
}

#[test]
#[should_panic]
fn test_var_assign_other_type() {
    run_test("a := 1; a = true;", None);
}

#[test]
#[should_panic]
fn test_const_assign() {
    run_test("a :: 1; a = 2;", None);
}
