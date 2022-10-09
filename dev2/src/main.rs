#![feature(associated_type_defaults)]

mod ast;
mod children;
mod leggevm;
mod linker;
mod macros;
mod node;
mod state;
mod types;

fn main() {
    leggevm::run();
}
