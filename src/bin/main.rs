use leggevm::run_code;
use leggevm::LogLevel;
use std::fs::File;
use std::io::prelude::*;
use std::process::exit;

fn main() {
    let filename = "main.bc";
    let mut f = File::open(filename).expect("file not found");

    let mut contents = String::new();
    f.read_to_string(&mut contents)
        .expect("something went wrong reading the file");

    let res = run_code(contents, LogLevel::LogEval, &|v| println!("{:?}", v));
    if res.is_some() {
        exit(0);
    } else {
        exit(1);
    }
}
