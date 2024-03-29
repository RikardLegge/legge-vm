use leggevm::testing::{bc_test, bc_test_should_fail};
use leggevm::vm::Value::*;

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

bc_test! {test_returned_closure_bind_arg_1 {
    wrap :: fn(f: Fn(int) -> int) -> Fn() -> int {
        return fn() -> int {
            v :: 21;
            return f(v);
        };
    }
    val :: wrap(fn(v: int) -> int {return v*2;});
    exit(val());
} == Int(42) }

bc_test! {test_returned_closure_bind_arg_2 {
    wrap :: fn(f: Fn(int) -> int) -> Fn() -> int {
        v :: 21;
        return fn() -> int {
            return f(v);
        };
    }
    val :: wrap(fn(v: int) -> int {return v*2;});
    exit(val());
} == Int(42) }

bc_test! {test_returned_closure_bind_arg_3 {
    wrap :: fn(f: Fn(int) -> int) -> Fn() -> int {
        ff :: f;
        v :: 21;
        return fn() -> int {
            return ff(v);
        };
    }
    val :: wrap(fn(v: int) -> int {return v*2;});
    exit(val());
} == Int(42) }

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
    import math.sin;
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

bc_test! {test_custom_type_nested_returned_by_fn {
    A -> type {
       value: int
    }
    B -> type {
       a: A
    }
    factory :: fn (new_a: Fn(int) -> A) -> Fn(int) -> B {
        return fn(value: int) -> B {
            b := B();
            b.a = new_a(value);
            return b;
        };
    }
    new :: factory(fn(v: int) -> A {
        a := A();
        a.value = v;
        return a;
    });
    exit(new(42));
} == Struct(vec![Struct(vec![Int(42)])]) }

bc_test! {test_associated_function {
    A -> type {
       value: int
    }

    A.double :: (self: A) -> A {
        self.value = self.value * 2;
        return self
    }

    a := A();
    a.value = 21
    b :: A.double()
    exit(b);
} == Struct(vec![Int(42)]) }

bc_test! {test_static_associated_function {
    A -> type {
       value: int
    }

    A.magic :: fn() -> A {
        a := A();
        a.value = 42
        return a;
    }

    a := A::magic();
    exit(a);
} == Struct(vec![Int(42)]) }

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
