#![feature(nll)]
#![feature(test)]

extern crate test;
extern crate core;

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
    let functions = load_foreign_functions();

    let mut contents = String::new();
    f.read_to_string(&mut contents)
        .expect("something went wrong reading the file");

    let tokens = Tokenizer::parse(&mut contents.chars().peekable().into_iter());
    println!("{:?}", tokens);

    let ast = Ast::from_tokens(&mut tokens.into_iter().peekable());
    println!("{:?}", ast);

    let bytecode = Bytecode::from_ast(&ast, &functions);
    println!("{:?}", bytecode);

    let encoded = serialize(&bytecode).unwrap();
    let bytecode: Bytecode = deserialize(&encoded[..]).unwrap();

    let mut interpreter = Interpreter::new(&functions);
    interpreter.run(&bytecode);
}

#[cfg(test)]
mod tests {
    use super::*;
    use test::Bencher;
    use token::Token;

    fn read() -> String {
        let filename = "main.bc";
        let mut f = File::open(filename).expect("file not found");

        let mut contents = String::new();
        f.read_to_string(&mut contents)
            .expect("something went wrong reading the file");
        contents
    }

    fn parse(contents: &str) -> Vec<Token> {
        Tokenizer::parse(&mut contents.chars().peekable().into_iter())
    }

    fn ast(tokens: Vec<Token>) -> Ast {
        Ast::from_tokens(&mut tokens.into_iter().peekable())
    }

    #[bench]
    fn bench_parse(b: &mut Bencher) {
        let contents = read();
        b.iter(|| parse(&contents));
    }

    #[bench]
    fn bench_ast(b: &mut Bencher) {
        let tokens = parse(&read());
        b.iter(|| ast(tokens.to_vec()));
    }

    #[bench]
    fn bench_ast_vec_copy(b: &mut Bencher) {
        let tokens = parse(&read());
        b.iter(|| tokens.to_vec());
    }

    #[bench]
    fn bench_interp(b: &mut Bencher) {
        let tokens = parse(&read());
        let ast = ast(tokens.to_vec());
        let functions = load_foreign_functions();
        let bytecode = Bytecode::from_ast(&ast, &functions);

        b.iter(|| Interpreter::new(&functions).run(&bytecode));
    }

    #[bench]
    fn bench_interp_native(b: &mut Bencher) {
        let add = |a,b| {println!("{}", a+b)};
        let one = || {1};
        b.iter(|| {
            {
                let a = 2 * (2 + 3);
                println!("{}", a);
                let a = 11;
                let b = 12;
                add(a, b);

                {
                    let mut a = one();
                    a = a + 2;
                    println!("{}", a+b*10);
                }
            }

        });
    }
}