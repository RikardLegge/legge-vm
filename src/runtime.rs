use crate::ast::NodeType;
use crate::bytecode::OP;
use crate::interpreter::{InterpError, InterpResult, Interpreter, Value};

pub type FunctionReturn = InterpResult<Option<Value>>;
pub type RuntimeFunction = &'static dyn Fn(&mut Interpreter, &mut Vec<Value>) -> FunctionReturn;

pub struct Runtime {
    pub functions: Vec<RuntimeFunctionDefinition>,
}

pub struct RuntimeFunctionDefinition {
    pub name: String,
    pub arguments: Vec<NodeType>,
    pub returns: NodeType,
    pub tp: NodeType,
    pub function: RuntimeFunction,
}

pub fn get() -> Runtime {
    use crate::ast::NodeType::*;
    let functions = vec![
        ff("log", vec![VarArg(Box::new(Any))], Void, &log),
        ff("assert", vec![Any, Any], Void, &assert),
        ff("exit", vec![Any], Void, &exit),
    ];
    Runtime { functions }
}

fn ff(
    name: &str,
    arguments: Vec<NodeType>,
    returns: NodeType,
    function: RuntimeFunction,
) -> RuntimeFunctionDefinition {
    let tp = NodeType::Fn(arguments.clone(), Box::new(returns.clone()));
    RuntimeFunctionDefinition {
        name: name.to_string(),
        arguments,
        returns,
        function,
        tp,
    }
}

fn assert(_: &mut Interpreter, args: &mut Vec<Value>) -> FunctionReturn {
    if args.len() != 2 {
        panic!("Assert must be called with two arguments")
    }
    if args[0] != args[1] {
        panic!("assertion failed: {:?} != {:?}", args[0], args[1])
    }
    Ok(None)
}

fn log(_: &mut Interpreter, args: &mut Vec<Value>) -> FunctionReturn {
    let last_arg = args.pop();
    for arg in args {
        print!("{:?}, ", arg);
    }
    if let Some(arg) = last_arg {
        print!("{:?}", arg);
    }
    println!();
    Ok(None)
}

fn exit(interp: &mut Interpreter, args: &mut Vec<Value>) -> FunctionReturn {
    if args.len() != 1 {
        Err(InterpError::new("Exit must be called with one argument"))
    } else {
        let value = args.pop().unwrap();
        interp.pop_stack_count(interp.stack.len())?;
        interp.execute(&OP::PushImmediate(value.into_bytecode().unwrap()))?;
        interp.execute(&OP::Yield)?;
        interp.exit();
        Ok(None)
    }
}
