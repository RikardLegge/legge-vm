use leggevm::vm::SystemFileStore;
use leggevm::LogLevel;
use leggevm::{run_code, Path};
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

    let store = SystemFileStore::new();
    let path = Path::try_new(
        filename
            .trim_end_matches(".bc")
            .split("/")
            .map(|s| s.to_string())
            .collect(),
    )
    .unwrap();

    match run_code(store, path, log_level, true, &|v| println!("{:?}", v)) {
        Ok(()) => exit(0),
        Err(err) => {
            println!("{}", err);
            exit(1)
        }
    }
}
