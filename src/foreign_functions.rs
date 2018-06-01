use interpreter::{Interpreter, InterpResult};
use bytecode::Bytecode;

pub struct ForeignFunction {
    pub name: String,
    pub arguments: i32,
    pub returns: i32,
    pub function: &'static Fn(&mut Interpreter, &Bytecode) -> InterpResult
}

pub fn log(interp: &mut Interpreter, _bytecode: &Bytecode) -> InterpResult {
    let count = interp.pop_stack()?;
    for _ in 0..count {
        let arg = interp.pop_stack()?;
        print!("{:?}", arg);
    }
    println!("");
    Ok(())
}