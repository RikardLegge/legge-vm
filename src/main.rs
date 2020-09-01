// use crate::interpreter::InterpLogLevel;
// use bincode::{deserialize, serialize};
// use bytecode::Bytecode;
use crate::bytecode::Value;
use interpreter::{InterpLogLevel, Interpreter};
use std::fs::File;
use std::io::prelude::*;
use token::Tokenizer;

mod ast;
mod bytecode;
mod debug;
mod interpreter;
mod runtime;
mod token;

fn main() {
    let filename = "main.bc";
    let mut f = File::open(filename).expect("file not found");

    let mut contents = String::new();
    f.read_to_string(&mut contents)
        .expect("something went wrong reading the file");

    run_code(contents, true, &|v| println!("{:?}", v))
}

fn run_code<F>(code: String, logging: bool, interrupt: F)
where
    F: Fn(Value),
{
    let mut timing = debug::Timing::default();
    let runtime = runtime::get();

    let start = debug::start_timer();
    let tokens = Tokenizer::parse(code.chars());
    timing.token = debug::stop_timer(start);

    let (ast, ast_timing) = ast::from_tokens(tokens.into_iter(), &runtime).unwrap();
    if logging {
        println!("{:?}", ast);
    }
    timing.ast = ast_timing;

    let start = debug::start_timer();
    let bytecode = bytecode::from_ast(&ast);
    timing.bytecode = debug::stop_timer(start);
    if logging {
        println!("{:?}", bytecode);
    }

    // let encoded = serialize(&bytecode).unwrap();
    // let bytecode: Bytecode = deserialize(&encoded[..]).unwrap();
    //
    let log_level = if logging {
        InterpLogLevel::LogEval
    } else {
        InterpLogLevel::LogNone
    };
    let mut interpreter = Interpreter::new(&runtime);
    interpreter.set_log_level(log_level);
    interpreter.interrupt = &interrupt;

    let start = debug::start_timer();
    timing.instructions = interpreter.run(&bytecode);
    timing.interpreter = debug::stop_timer(start);
    timing.avg_instruction = timing.interpreter / timing.instructions as u32;

    if logging {
        dbg!(timing);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bytecode::Value::*;
    use std::cell::RefCell;
    use std::rc::Rc;

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
        run_test("if(1 == 1) {exit(1);} exit(0);", Some(Int(1)));
    }

    #[test]
    fn test_if_fales() {
        run_test("if(1 == 2) {exit(1);} exit(0);", Some(Int(0)));
    }
}
