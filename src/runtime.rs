use crate::ast::NodeType;
use crate::bytecode::Value::Int;
use crate::bytecode::{Value, OP};
use crate::interpreter::{Err, Interpreter, Result};

pub type FunctionReturn = Result<Option<Value>>;
pub type Function = &'static dyn Fn(&mut Interpreter, &mut Vec<Value>) -> FunctionReturn;

pub struct Runtime {
    pub functions: Vec<FunctionDefinition>,
}

pub struct FunctionDefinition {
    pub namespace: String,
    pub name: String,
    pub arguments: Vec<NodeType>,
    pub returns: NodeType,
    pub tp: NodeType,
    pub function: Function,
}

pub fn std() -> Runtime {
    use crate::ast::NodeType::*;
    let functions = vec![
        ff("std", "int", vec![Any], Int, &to_int),
        ff("std", "float", vec![Any], Float, &to_float),
        ff(
            "std",
            "print",
            vec![VarArg {
                args: Box::new(Any),
            }],
            Void,
            &print,
        ),
        ff("std", "assert", vec![Any, Any], Void, &assert),
        ff("std", "exit", vec![Any], Void, &exit),
        ff(
            "std",
            "touch",
            vec![VarArg {
                args: Box::new(Any),
            }],
            Void,
            &touch,
        ),
        ff("math", "sin", vec![Int], Int, &sin),
    ];
    Runtime { functions }
}

fn ff(
    namespace: &str,
    name: &str,
    arguments: Vec<NodeType>,
    returns: NodeType,
    function: Function,
) -> FunctionDefinition {
    let tp = NodeType::Fn {
        args: arguments.clone(),
        returns: Box::new(returns.clone()),
    };
    FunctionDefinition {
        namespace: namespace.into(),
        name: name.into(),
        arguments,
        returns,
        function,
        tp,
    }
}

fn to_int(_: &mut Interpreter, args: &mut Vec<Value>) -> FunctionReturn {
    if args.len() != 1 {
        panic!("int must be called with one arguments")
    }
    let result = match args[0] {
        Value::Int(int) => Value::Int(int),
        Value::Float(int) => Value::Int(int as isize),
        _ => panic!("invalid value type passed to int: {:?}", args[0]),
    };
    Ok(Some(result))
}

fn to_float(_: &mut Interpreter, args: &mut Vec<Value>) -> FunctionReturn {
    if args.len() != 1 {
        panic!("int must be called with one arguments")
    }
    let result = match args[0] {
        Value::Int(int) => Value::Float(int as f64),
        Value::Float(float) => Value::Float(float),
        _ => panic!("invalid value type passed to int: {:?}", args[0]),
    };
    Ok(Some(result))
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

fn print(_: &mut Interpreter, args: &mut Vec<Value>) -> FunctionReturn {
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

fn touch(_: &mut Interpreter, _: &mut Vec<Value>) -> FunctionReturn {
    Ok(None)
}

fn exit(interp: &mut Interpreter, args: &mut Vec<Value>) -> FunctionReturn {
    if args.len() == 1 {
        let value = args.pop().unwrap();
        interp.execute(&OP::PushImmediate(value), Default::default())?;
        interp.execute(&OP::Yield, Default::default())?;
        interp.exit();
        Ok(None)
    } else {
        Err(Err::new("Exit must be called with one argument"))
    }
}
