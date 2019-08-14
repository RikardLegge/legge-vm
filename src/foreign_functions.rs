use crate::interpreter::{ForeignInterpResult};

pub struct ForeignFunction {
    pub name: String,
    pub arguments: Option<usize>,
    pub returns: usize,
    pub function: &'static Fn(&mut Vec<i64>) -> ForeignInterpResult
}

pub fn load_foreign_functions() -> Vec<ForeignFunction> {
    return vec![
        ForeignFunction {name: "log".to_string(), arguments: None, returns: 0, function: &log},
        ForeignFunction {name: "assert".to_string(), arguments: Some(2), returns: 0, function: &assert}
    ];
}

fn assert(args: &mut Vec<i64>) -> ForeignInterpResult {
    if args.len() != 2 {
        panic!("Assert must be called with two arguments")
    }
    if args[0] != args[1] {
        panic!("assertion failed: {} != {}", args[0], args[1])
    }
    Ok(vec![])
}

fn log(args: &mut Vec<i64>) -> ForeignInterpResult {
    let last_arg = args.pop();
    for arg in args {
        print!("{:?}, ", arg);
    }
    if let Some(arg) = last_arg {
        print!("{:?}", arg);
    }

    println!();
    Ok(vec![])
}