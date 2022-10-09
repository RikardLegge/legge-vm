#![feature(associated_type_defaults)]

mod ast;
mod legge_vm;
mod macros;
mod node;
mod types;

fn main() {
    legge_vm::run();
}
