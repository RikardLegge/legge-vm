use interpreter::{Interpreter, InterpResult};
use bytecode::Bytecode;

pub struct ForeignFunction {
    pub name: String,
    pub arguments: i64,
    pub returns: usize,
    pub function: &'static Fn(&mut Interpreter, &Bytecode) -> InterpResult
}

pub fn load_foreign_functions() -> Vec<ForeignFunction> {
    let mut foreign_functions = Vec::new();
    foreign_functions.push(ForeignFunction {name: "log".to_string(), arguments: -1, returns: 0, function: &log});
    foreign_functions.push(ForeignFunction {name: "one".to_string(), arguments: 0, returns: 1, function: &one});
    foreign_functions
}

fn one(interp: &mut Interpreter, _bytecode: &Bytecode) -> InterpResult {
    interp.get_foreign_function_arguments()?;
    interp.push_stack(1);
    Ok(())
}

fn log(interp: &mut Interpreter, _bytecode: &Bytecode) -> InterpResult {
    let args = interp.get_foreign_function_arguments()?;
    for arg in args {
        print!("{:?}", arg);
    }
    println!("");
    Ok(())
}