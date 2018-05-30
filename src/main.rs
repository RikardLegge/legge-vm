#![feature(nll)]

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

