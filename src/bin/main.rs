use leggevm::run_code;
use leggevm::LogLevel;
use std::env::args;
use std::fs::File;
use std::io::prelude::*;
use std::process::exit;

fn main() {
    let args = args().skip(1).collect::<Vec<String>>();
    if args.len() == 0 {
        panic!("First argument must be the file name");
    }
    let filename = &args[0];
    let mut f = File::open(filename).expect("file not found");

    let mut contents = String::new();
    f.read_to_string(&mut contents)
        .expect("something went wrong reading the file");

    let res = run_code(
        filename.trim_end_matches(".bc").to_string(),
        contents,
        LogLevel::LogEval,
        &|v| println!("{:?}", v),
    );
    if res.is_some() {
        exit(0);
    } else {
        exit(1);
    }
}
