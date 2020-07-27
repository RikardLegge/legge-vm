// use crate::interpreter::InterpLogLevel;
use ast::Ast;
// use bincode::{deserialize, serialize};
// use bytecode::Bytecode;
// use foreign_functions::load_foreign_functions;
// use interpreter::Interpreter;
use std::fs::File;
use std::io::prelude::*;
use std::time::{Duration, SystemTime};
use token::Tokenizer;

mod ast;
// mod bytecode;
// mod foreign_functions;
// mod interpreter;
mod token;

fn main() {
    let filename = "main.bc";
    let mut f = File::open(filename).expect("file not found");

    let mut contents = String::new();
    f.read_to_string(&mut contents)
        .expect("something went wrong reading the file");

    run_code(contents)
}

#[derive(Default, Debug)]
struct Timing {
    token: Duration,
    ast: Duration,
    bytecode: Duration,
    interpreter: Duration,
    instructions: usize,
}

fn run_code(code: String) {
    let mut timing = Timing::default();

    let start = SystemTime::now();
    let tokens = Tokenizer::parse(code.chars());
    timing.token = SystemTime::now().duration_since(start).unwrap();

    let start = SystemTime::now();
    let _ast = Ast::from_tokens(tokens.into_iter()).unwrap();
    timing.ast = SystemTime::now().duration_since(start).unwrap();
    dbg!(_ast);

    // let start = SystemTime::now();
    // let functions = load_foreign_functions();
    // let bytecode = Bytecode::from_ast(&ast, &functions);
    // timing.bytecode = SystemTime::now().duration_since(start).unwrap();
    //
    // let encoded = serialize(&bytecode).unwrap();
    // let bytecode: Bytecode = deserialize(&encoded[..]).unwrap();
    //
    // let mut interpreter = Interpreter::new(&functions);
    // interpreter.set_log_level(InterpLogLevel::LogNone);
    //
    // let start = SystemTime::now();
    // timing.instructions = interpreter.run(&bytecode);
    // timing.interpreter = SystemTime::now().duration_since(start).unwrap();

    dbg!(timing);
}

#[cfg(test)]
mod tests {
    use super::*;

    fn run_test(code: &str) {
        run_code(code.into())
    }

    #[test]
    fn test_assert_true() {
        run_test(
            "
            a :: 1;
            assert(a, 1);
        ",
        );
    }

    #[test]
    fn test_recursion() {
        run_test(
            "
            rec :: fn (i) {
                if(i == 0) {
                    return;
                }
                rec(i - 1);
            }
            rec(5);
        ",
        );
    }

    #[test]
    #[should_panic]
    fn test_assert_false() {
        run_test(
            "
            a :: 1;
            assert(a, 2);
        ",
        );
    }

    #[test]
    fn single_argument_function_call() {
        run_test(
            "
            main :: fn(a) {
                assert(a, 1);
            }
            main(1);
        ",
        );
    }

    #[test]
    #[should_panic]
    fn wrong_argument_function_call() {
        run_test(
            "
            main :: fn(a) {}
            main(1,2);
        ",
        );
    }

    #[test]
    fn function_early_return() {
        run_test(
            "
            main :: fn() {
                return;
                assert(1,2);
            }
            main();
        ",
        );
    }

    #[test]
    fn function_void_return_value_ok() {
        run_test(
            "
            main :: fn() {
                assert(1,1);
                return;
            }
            main();
        ",
        );
    }

    #[test]
    #[should_panic]
    fn function_void_return_value_err() {
        run_test(
            "
            main :: fn() {
                assert(1,2);
                return;
            }
            main();
        ",
        );
    }

    #[test]
    #[should_panic]
    fn turn_value_err() {
        run_test(
            "
            main :: fn() -> int {
                return 1;
            }
            a :: main();
        ",
        );
    }

    #[test]
    fn function_nested_scopes() {
        run_test(
            "
            main :: fn() {
                a := 1;
                {
                    b := 2;
                    return;
                }
                c := 3;
            }
            main();
        ",
        );
    }

    #[test]
    fn if_ok() {
        run_test(
            "
            if (1 == 1) {
                assert(1,1);
            }
        ",
        );
    }

    #[test]
    #[should_panic]
    fn if_err() {
        run_test(
            "
            if (1 == 1) {
                assert(1,2);
            }
        ",
        );
    }

    #[test]
    fn if_not_ok() {
        run_test(
            "
            if (1 == 2) {
                assert(1,2);
            }
        ",
        );
    }

    #[test]
    #[should_panic]
    fn if_not_err() {
        run_test(
            "
            if (1 == 2) {
                assert(1,2);
            }
            assert(1,2);
        ",
        );
    }

    #[test]
    fn if_nested_ok() {
        run_test(
            "
            if (1 == 1) {
                if (1 == 2) {
                    assert(1,2);
                }
            }
        ",
        );
    }

    #[test]
    #[should_panic]
    fn if_nested_err() {
        run_test(
            "
            if (1 == 1) {
                if (1 == 1) {
                    assert(1,2);
                }
            }
        ",
        );
    }

    #[test]
    fn loop_break_ok() {
        run_test(
            "
            n := 0;
            loop {
                if (n == 10) {
                    break;
                }
                n = n + 1;
            }
            assert(n, 10);
        ",
        );
    }

    //    #[test]
    //    fn recursion() {
    //        run_test("
    //            loop :: fn(n) {
    //                if (n == 0) {
    //                    return 1;
    //                }
    //                return loop(n+1) + 1;
    //            }
    //            assert(loop(10),10);
    //        ");
    //    }
}

//    use test::Bencher;

//    fn read() -> String {
//        let filename = "main.bc";
//        let mut f = File::open(filename).expect("file not found");
//
//        let mut contents = String::new();
//        f.read_to_string(&mut contents)
//            .expect("something went wrong reading the file");
//        contents
//    }
//
//    fn parse(contents: &str) -> Vec<Token> {
//        Tokenizer::parse(&mut contents.chars().peekable().into_iter())
//    }
//
//    fn ast(tokens: Vec<Token>) -> Ast {
//        Ast::from_tokens(&mut tokens.into_iter().peekable())
//    }

//    #[bench]
//    fn bench_parse(b: &mut Bencher) {
//        let contents = read();
//        b.iter(|| parse(&contents));
//    }
//
//    #[bench]
//    fn bench_ast(b: &mut Bencher) {
//        let tokens = parse(&read());
//        b.iter(|| ast(tokens.to_vec()));
//    }
//
//    #[bench]
//    fn bench_ast_vec_copy(b: &mut Bencher) {
//        let tokens = parse(&read());
//        b.iter(|| tokens.to_vec());
//    }
//
//    #[bench]
//    fn bench_interp(b: &mut Bencher) {
//        let tokens = parse(&read());
//        let ast = ast(tokens.to_vec());
//        let functions = load_foreign_functions();
//        let bytecode = Bytecode::from_ast(&ast, &functions);
//
//        b.iter(|| Interpreter::new(&functions).run(&bytecode));
//    }
//
//    #[bench]
//    fn bench_interp_native(b: &mut Bencher) {
//        let add = |a,b| {println!("{}", a+b)};
//        let one = || {1};
//        b.iter(|| {
//            {
//                let a = 2 * (2 + 3);
//                println!("{}", a);
//                let a = 11;
//                let b = 12;
//                add(a, b);
//
//                {
//                    let mut a = one();
//                    a = a + 2;
//                    println!("{}", a+b*10);
//                }
//            }
//
//        });
//    }
