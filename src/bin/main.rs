use leggevm::LogLevel;
use leggevm::{run_code, Path};
use std::fs::File;
use std::io::prelude::*;
use std::process::exit;

use clap::{App, Arg};

fn main() {
    let matches = App::new("Legge VM")
        .version("0.1.0")
        .author("Rikard Legge <rikard@legge.se>")
        .about("Compiler and virtual machine for the Legge language")
        .arg(
            Arg::new("v")
                .short('v')
                .multiple_occurrences(true)
                .about("Sets the level of verbosity"),
        )
        .arg(
            Arg::new("INPUT")
                .about("Sets the input file to use")
                .required(true)
                .index(1),
        )
        .get_matches();

    let log_level = match matches.occurrences_of("v") {
        0 => LogLevel::LogNone,
        1 => LogLevel::LogDebug,
        2 => LogLevel::LogTiming,
        3 => LogLevel::LogEval,
        _ => panic!("invalid log level, maximum log level is 3"),
    };

    let filename = match matches.value_of("INPUT") {
        Some(filename) => filename,
        None => panic!("No input file provided"),
    };

    let mut f = File::open(filename).expect("file not found");

    let mut contents = String::new();
    f.read_to_string(&mut contents)
        .expect("something went wrong reading the file");

    let path = Path::new(vec![filename.trim_end_matches(".bc").to_string()]);
    match run_code(path, contents, log_level, &|v| println!("{:?}", v)) {
        Ok(()) => exit(0),
        Err(err) => {
            println!("{}", err);
            exit(1)
        }
    }
}
