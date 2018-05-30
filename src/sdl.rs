use interpreter::Interpreter;
use bytecode::Bytecode;
use ast::AstPrimitives;

pub fn log(interp: &mut Interpreter, _bytecode: &Bytecode) -> Option<()> {
    let args_count = interp.stack.pop()?;
    if let AstPrimitives::Int(count) = args_count {
        for _ in 0..count {
            let arg = interp.stack.pop()?;
            print!("{:?}", arg);
        }
        println!("");
        Some(())
    } else {
        None
    }
}