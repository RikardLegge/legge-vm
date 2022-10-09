#![feature(associated_type_defaults)]

mod ast;
mod leggevm;
mod macros;
mod node;
mod types;

fn main() {
    leggevm::run();
}
