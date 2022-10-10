#![feature(associated_type_defaults)]

mod ast;
mod checker;
mod children;
mod leggevm;
mod linker;
mod macros;
mod node;
mod state;
mod token;
mod tree_builder;
mod types;

fn main() {
    leggevm::run();
}
