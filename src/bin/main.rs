use leggevm::run_code;
use std::fs::File;
use std::io::prelude::*;

fn main() {
    let filename = "main.bc";
    let mut f = File::open(filename).expect("file not found");

    let mut contents = String::new();
    f.read_to_string(&mut contents)
        .expect("something went wrong reading the file");

    run_code(contents, true, &|v| println!("{:?}", v))
}
