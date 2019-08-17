#[macro_use]
extern crate serde_derive;
extern crate bincode;

use std::io::prelude::*;
use std::fs::File;

mod token;
mod ast;
mod bytecode;
mod interpreter;
mod foreign_functions;

use token::Tokenizer;
use ast::Ast;
use bytecode::Bytecode;
use interpreter::Interpreter;
use foreign_functions::load_foreign_functions;
use bincode::{serialize, deserialize};


fn main() {
    let filename = "main.bc";
    let mut f = File::open(filename).expect("file not found");

    let mut contents = String::new();
    f.read_to_string(&mut contents)
        .expect("something went wrong reading the file");

    run_code(contents)
}

fn run_code(code: String) {
    let functions = load_foreign_functions();

    let tokens = Tokenizer::parse(&mut code.chars().peekable().into_iter());
    dbg!(&tokens);

    let ast = Ast::from_tokens(&mut tokens.into_iter().peekable());
    dbg!(&ast);

    let bytecode = Bytecode::from_ast(&ast, &functions);
    dbg!(&bytecode);

    let encoded = serialize(&bytecode).unwrap();
    let bytecode: Bytecode = deserialize(&encoded[..]).unwrap();

    let mut interpreter = Interpreter::new(&functions);
    interpreter.run(&bytecode);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_assert_true() {
        run_code("
            a :: 1;
            assert(a, 1);
        ".into());
    }

    #[test]
    #[should_panic]
    fn test_assert_false() {
        run_code("
            a :: 1;
            assert(a, 2);
        ".into());
    }

    #[test]
    fn single_argument_function_call() {
        run_code("
            main :: fn(a) {
                assert(a, 1);
            }
            main(1);
        ".into());
    }

    #[test]
    #[should_panic]
    fn wrong_argument_function_call() {
        run_code("
            main :: fn(a) {}
            main(1,2);
        ".into());
    }

    #[test]
    fn function_early_return() {
        run_code("
            main :: fn() {
                return;
                assert(1,2);
            }
            main();
        ".into());
    }

    #[test]
    fn function_void_return_value_ok() {
        run_code("
            main :: fn() {
                assert(1,1);
                return;
            }
            main();
        ".into());
    }

    #[test]
    #[should_panic]
    fn function_void_return_value_err() {
        run_code("
            main :: fn() {
                assert(1,2);
                return;
            }
            main();
        ".into());
    }

    #[test]
    fn function_return_value_ok() {
        run_code("
            main :: fn() -> int {
                return 1;
            }
            a :: main();
            assert(a, 1);
        ".into());
    }

    #[test]
    #[should_panic]
    fn function_return_value_err() {
        run_code("
            main :: fn() -> int {
                return 1;
            }
            a :: main();
            assert(a, 2);
        ".into());
    }

    #[test]
    fn function_return_value_as_argument_ok() {
        run_code("
            main :: fn() -> int {
                return 1;
            }
            assert(main(), 1);
        ".into());
    }

    #[test]
    #[should_panic]
    fn function_return_value_as_argument_err() {
        run_code("
            main :: fn() -> int {
                return 1;
            }
            assert(main(), 2);
        ".into());
    }

    #[test]
    #[should_panic]
    fn function_return_value_without_location() {
        run_code("
            main :: fn() -> int {
                return 1;
            }
            main();
        ".into());
    }

    #[test]
    fn if_ok() {
        run_code("
            if (1 == 1) {
                assert(1,1);
            }
        ".into());
    }

    #[test]
    #[should_panic]
    fn if_err() {
        run_code("
            if (1 == 1) {
                assert(1,2);
            }
        ".into());
    }

    #[test]
    fn if_not_ok() {
        run_code("
            if (1 == 2) {
                assert(1,2);
            }
        ".into());
    }

    #[test]
    #[should_panic]
    fn if_not_err() {
        run_code("
            if (1 == 2) {
                assert(1,2);
            }
            assert(1,2);
        ".into());
    }

    #[test]
    fn if_nested_ok() {
        run_code("
            if (1 == 1) {
                if (1 == 2) {
                    assert(1,2);
                }
            }
        ".into());
    }

    #[test]
    #[should_panic]
    fn if_nested_err() {
        run_code("
            if (1 == 1) {
                if (1 == 1) {
                    assert(1,2);
                }
            }
        ".into());
    }

//    #[test]
//    fn recursion() {
//        run_code("
//            loop :: fn(n) {
//                if (n == 0) {
//                    return 1;
//                }
//                return loop(n+1) + 1;
//            }
//            assert(loop(10),10);
//        ".into());
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
