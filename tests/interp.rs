use std::cell::RefCell;
use std::rc::Rc;

use leggevm::bytecode::Value;
use leggevm::bytecode::Value::*;
use leggevm::{run_code, LogLevel};

fn run_test(code: &str, expected_result: Option<Value>) {
    let code = format!("import exit;{}", code);
    let result = Rc::new(RefCell::new(None));
    let assign_result = result.clone();
    run_code(code.into(), LogLevel::LogNone, move |v| {
        *assign_result.borrow_mut() = Some(v);
    }).unwrap();
    let result = result.borrow();
    assert_eq!(*result, expected_result);
}

#[test]
fn test_variable() {
    run_test("a :: 1; exit(a);", Some(Int(1)));
}

#[test]
fn test_variable_parenthesis() {
    run_test("a :: (1); exit(a);", Some(Int(1)));
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
#[should_panic]
fn test_redeclare() {
    run_test("a :: 1; a :: 1; exit(a);", Some(Int(1)));
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
fn test_access_non_local_value() {
    run_test(
        "
        val :: 1;
        get_val :: fn() -> int {
            return val; 
        }
        exit(get_val());",
        Some(Int(1)),
    );
}

#[test]
fn test_modify_non_local_value() {
    run_test(
        "
        val := 1;
        change_val :: fn() {
            val = 2; 
        }
        get_val :: fn() -> int {
            return val; 
        }
        change_val();
        exit(get_val());",
        Some(Int(2)),
    );
}

#[test]
fn test_returned_closure() {
    run_test(
        "
        incrementer :: fn() -> Fn() -> int {
            val := 0; 
            return fn() -> int {
                val = val + 1;
                return val;
            };
        }
        inc := incrementer();
        inc();
        inc();
        exit(inc());",
        Some(Int(3)),
    );
}

#[test]
fn text_nested_scope() {
    run_test(
        "
        proxy :: fn() {
            val := 0;
            real :: fn() {
                exit(val);
            }
            real();
        }
        proxy();",
        Some(Int(0)),
    );
}

#[test]
fn text_nested_arg_scope() {
    run_test(
        "
        proxy :: fn(val: int) {
            real :: fn() {
                exit(val);
            }
            real();
        }
        proxy(0);",
        Some(Int(0)),
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
fn test_call_builtin() {
    run_test("import sin; exit(sin(sin(sin(0))));", Some(Int(0)));
}

#[test]
fn test_call_multiple() {
    run_test(
        "\
        inc :: fn(a: int) -> int {\
            return a + 1;
        } \
        exit(inc(inc(inc(0))));",
        Some(Int(3)),
    );
}

#[test]
fn test_loop() {
    run_test(
        "i := 0; loop { if(i == 100) {break;} i = i + 1;} exit(i);",
        Some(Int(100)),
    );
}

#[test]
fn test_custom_type() {
    run_test(
        "
         A -> type {
            a: int
         }
         a :: A();
        exit(a);",
        Some(Struct(vec![Int(0)])),
    );
}

#[test]
fn test_custom_type_set() {
    run_test(
        "
         A -> type {
            a: int
         }
         a := A();
         a.a = 10;
        exit(a);",
        Some(Struct(vec![Int(10)])),
    );
}

#[test]
fn test_custom_type_get() {
    run_test(
        "
         A -> type {
            a: int
         }
         a :: A();
        exit(a.a);",
        Some(Int(0)),
    );
}

#[test]
fn test_custom_type_nested() {
    run_test(
        "
         A -> type {
            a: int
         }
         B -> type {
            to_a: A
        }
        b := B();
        b.to_a.a = 10;
        exit(b);",
        Some(Struct(vec![Struct(vec![Int(10)])])),
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

#[test]
#[should_panic]
fn test_invalid_return_value_type_1() {
    run_test(
        "
        incrementer :: fn() -> Fn() -> int {
            return fn() {};
        }
        inc := incrementer();
        val := inc();
        exit(val);",
        None,
    );
}

#[test]
#[should_panic]
fn test_invalid_return_value_type_2() {
    run_test(
        "
        incrementer :: fn() -> Fn() -> int {
            return fn() -> bool {return false};
        }
        inc := incrementer();
        val := inc();
        exit(val);",
        None,
    );
}

#[test]
fn test_valid_return_value_type() {
    run_test(
        "
        incrementer :: fn() -> Fn() -> int {
            return fn() -> int {return 1;};
        }
        inc := incrementer();
        val := inc();
        exit(val);",
        Some(Int(1)),
    );
}
