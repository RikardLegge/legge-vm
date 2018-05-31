#![feature(nll)]
#![feature(test)]

extern crate test;

use std::io::prelude::*;
use std::fs::File;

mod token;
mod ast;
mod bytecode;
mod interpreter;
mod sdl;

use token::parse_tokens;
use ast::Ast;
use bytecode::Bytecode;
use interpreter::Interpreter;

fn main() {
    let filename = "main.bc";
    let mut f = File::open(filename).expect("file not found");

    let mut contents = String::new();
    f.read_to_string(&mut contents)
        .expect("something went wrong reading the file");

    let tokens = parse_tokens(&mut contents.chars().peekable().into_iter());
    println!("{:?}", tokens);

    let ast = Ast::from_tokens(&mut tokens.into_iter().peekable());
    println!("{:?}", ast);

    let bytecode = Bytecode::from_ast(&ast);

    let mut interpreter = Interpreter::new();
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
        parse_tokens(&mut contents.chars().peekable().into_iter())
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
        let bytecode = Bytecode::from_ast(&ast);

        b.iter(|| Interpreter::new().run(&bytecode));
    }
}