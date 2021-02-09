use crate::ast::NodeType;
use crate::bytecode::Value::Int;
use crate::bytecode::{Value, OP};
use crate::interpreter::{Err, Interpreter, Result};

pub type FunctionReturn = Result<Option<Value>>;
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
        ff("sin", vec![Int], Int, &sin),
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

fn sin(_: &mut Interpreter, args: &mut Vec<Value>) -> FunctionReturn {
    if args.len() != 1 {
        panic!("Assert must be called with one arguments")
    }
    let result = match args[0] {
        Value::Int(angle) => (angle as f64).sin(),
        _ => panic!("invalid value type passed to sin: {:?}", args[0]),
    };
    Ok(Some(Int(result.round() as isize)))
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
    if args.len() == 1 {
        let value = args.pop().unwrap();
        interp.execute(&OP::PushImmediate(value))?;
        interp.execute(&OP::Yield)?;
        interp.exit();
        Ok(None)
    } else {
        Err(Err::new("Exit must be called with one argument"))
    }
}
