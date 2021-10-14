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
    }).expect("code should compile and run");
    let result = result.borrow();
    assert_eq!(*result, expected_result);
}

macro_rules! bc_test_should_fail {
    ( $name:ident $bc:tt) => {
        #[test]
        #[should_panic]
        fn $name() {
            run_test(stringify!($bc), None);
        }
    };
}

macro_rules! bc_test {
    ( $name:ident $bc:tt) => {
        #[test]
        fn $name() {
            run_test(stringify!($bc), None);
        }
    };
    ( $name:ident $bc:tt == $result:expr ) => {
        #[test]
        fn $name() {
            run_test(stringify!($bc), Some($result));
        }
    };
}

bc_test! {test_variable {
    a :: 1;
    exit(a);
} == Int(1) }

bc_test! {test_variable_parenthesis {
    a :: (1);
    exit(a);
} == Int(1) }

bc_test! {test_add {
    a :: 1;
    b :: 2;
    c :: a + b;
    exit(c);
} == Int(3) }

bc_test! {test_sub {
    a :: 1;
    b :: 2;
    c :: a - b;
    exit(c);
} == Int(-1) }

bc_test! {test_mult {
    a :: 2;
    b :: 3;
    c :: a * b;
    exit(c);
} == Int(6) }

bc_test! {test_div {
    a :: 1;
    b :: 2;
    c :: a / b;
    exit(c);
} == Int(0) }

bc_test_should_fail! {test_redeclare_const {
    a :: 1;
    a :: 1;
    exit(a);
}}

bc_test! {test_if_true {
    if(true) {
        exit(1);
    }
    exit(0);
} == Int(1) }

bc_test! {test_if_false {
    if(false) {
        exit(1);
    }
    exit(0);
} == Int(0) }

bc_test! {test_function {
    double :: fn(i: int) -> int {
        return 2 * i;
    }
    exit(double(2));
} == Int(4) }

bc_test! {test_pass_function {
    double :: fn(i: int) -> int {
        return 2 * i;
    }
    exec :: fn(num: int, map: Fn(int) -> int) -> int {
        return map(num);
    }
    exit(exec(2, double));
} == Int(4) }

bc_test! {test_access_non_local_value {
    val :: 1;
    get_val :: fn() -> int {
        return val;
    }
    exit(get_val());
} == Int(1) }

bc_test! {test_modify_non_local_value {
    val := 1;
    change_val :: fn() {
        val = 2;
    }
    get_val :: fn() -> int {
        return val;
    }
    change_val();
    exit(get_val());
} == Int(2) }

bc_test! {test_returned_closure {
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
    exit(inc());
} == Int(3) }

bc_test! {test_nested_scope {
    proxy :: fn() {
        val := 0;
        real :: fn() {
            exit(val);
        }
        real();
    }
    proxy();
} == Int(0) }

bc_test! {text_nested_arg_scope {
    proxy :: fn(val: int) {
        real :: fn() {
            exit(val);
        }
        real();
    }
    proxy(0);
} == Int(0) }

bc_test! {test_comments_after_return {
    exec :: fn() -> int {
        return 4;
        // a comment
    }
    exit(exec());
} == Int(4) }

bc_test! {test_return_function {
    get :: fn() -> Fn(int) -> int {
        return fn(i: int) -> int { return 2 * i; };
    }
    func :: get();
    exit(func(2));
} == Int(4) }

bc_test! {test_var_assign {
    a := 1;
    a = 2;
    exit(a);
} == Int(2) }

bc_test! {test_call_builtin {
    import sin;
    exit(sin(sin(sin(0))));
} == Int(0) }

bc_test! {test_call_multiple {
    inc :: fn(a: int) -> int {
        return a + 1;
    }
    exit(inc(inc(inc(0))));
} == Int(3) }

bc_test! {test_loop {
    i := 0;
    loop {
        if(i == 100) {
            break;
        }
        i = i + 1;
    }
    exit(i);
} == Int(100) }

bc_test! {test_custom_type {
    A -> type {
       a: int
    }
    a :: A();
    exit(a);
} == Struct(vec![Int(0)]) }

bc_test! {test_custom_type_set {
    A -> type {
       a: int
    }
    a := A();
    a.a = 10;
    exit(a);
} == Struct(vec![Int(10)]) }

bc_test! {test_custom_type_get {
    A -> type {
       a: int
    }
    a :: A();
    exit(a.a);
} == Int(0) }

bc_test! {test_custom_type_nested {
    A -> type {
       a: int
    }
    B -> type {
       to_a: A
    }
    b := B();
    b.to_a.a = 10;
    exit(b);
} == Struct(vec![Struct(vec![Int(10)])]) }

bc_test_should_fail! {test_var_assign_other_type {
    a := 1;
    a = true;
}}

bc_test! {test_empty_type {
    a := 1;
    b : = 2;
    exit(a+b);
} == Int(3) }

bc_test_should_fail! {test_const_assign {
    a :: 1;
    a = 2;
}}

bc_test_should_fail! {test_invalid_return_value_type_1 {
    incrementer :: fn() -> Fn() -> int {
        return fn() {};
    }
    inc := incrementer();
    val := inc();
    exit(val);
}}

bc_test_should_fail! {test_invalid_return_value_type_2 {
    incrementer :: fn() -> Fn() -> int {
        return fn() -> bool {return false};
    }
    inc := incrementer();
    val := inc();
    exit(val);
}}

bc_test! {test_valid_return_value_type {
    incrementer :: fn() -> Fn() -> int {
        return fn() -> int {
            return 1;
        };
    }
    inc := incrementer();
    val := inc();
    exit(val);
} == Int(1) }
