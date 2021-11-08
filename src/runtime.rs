use crate::ast::NodeType;
use crate::bytecode::Value::Int;
use crate::bytecode::{Value, OP};
use crate::interpreter::{Err, Interpreter, Result};
use std::sync::Arc;

pub type FunctionReturn = Result<Option<Value>>;
pub type Function = &'static dyn Fn(&mut Interpreter, Vec<Value>) -> FunctionReturn;

pub struct Runtime {
    pub definitions: Arc<Vec<FunctionDefinition>>,
    pub functions: Vec<Function>,
}

#[derive(Debug)]
pub struct FunctionDefinition {
    pub module: String,
    pub name: String,
    pub arguments: Vec<NodeType>,
    pub returns: NodeType,
    pub tp: NodeType,
}

pub fn std() -> Runtime {
    use crate::ast::NodeType::*;
    let mut f = vec![];
    let mut d = vec![];
    ff(&mut f, &mut d, "std", "int", vec![Any], Int, &to_int);
    ff(&mut f, &mut d, "std", "float", vec![Any], Float, &to_float);
    ff(
        &mut f,
        &mut d,
        "std",
        "print",
        vec![VarArg {
            args: Box::new(Any),
        }],
        Void,
        &print,
    );
    ff(
        &mut f,
        &mut d,
        r"std",
        "assert",
        vec![Any, Any],
        Void,
        &assert,
    );
    ff(&mut f, &mut d, "std", "exit", vec![Any], Void, &exit);
    ff(
        &mut f,
        &mut d,
        "std",
        "touch",
        vec![VarArg {
            args: Box::new(Any),
        }],
        Void,
        &touch,
    );
    ff(&mut f, &mut d, "math", "sin", vec![Int], Int, &sin);
    Runtime {
        functions: f,
        definitions: Arc::new(d),
    }
}

fn ff(
    functions: &mut Vec<Function>,
    definitions: &mut Vec<FunctionDefinition>,
    namespace: &str,
    name: &str,
    arguments: Vec<NodeType>,
    returns: NodeType,
    function: Function,
) {
    let tp = NodeType::Fn {
        args: arguments.clone(),
        returns: Box::new(returns.clone()),
    };
    let definition = FunctionDefinition {
        module: namespace.into(),
        name: name.into(),
        arguments,
        returns,
        tp,
    };
    definitions.push(definition);
    functions.push(function);
}

fn to_int(_: &mut Interpreter, args: Vec<Value>) -> FunctionReturn {
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

fn to_float(_: &mut Interpreter, args: Vec<Value>) -> FunctionReturn {
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

fn sin(_: &mut Interpreter, args: Vec<Value>) -> FunctionReturn {
    if args.len() != 1 {
        panic!("Assert must be called with one arguments")
    }
    let result = match args[0] {
        Value::Int(angle) => (angle as f64).sin(),
        _ => panic!("invalid value type passed to sin: {:?}", args[0]),
    };
    Ok(Some(Int(result.round() as isize)))
}

fn assert(_: &mut Interpreter, args: Vec<Value>) -> FunctionReturn {
    if args.len() != 2 {
        panic!("Assert must be called with two arguments")
    }
    if args[0] != args[1] {
        panic!("assertion failed: {:?} != {:?}", args[0], args[1])
    }
    Ok(None)
}

fn print(_: &mut Interpreter, mut args: Vec<Value>) -> FunctionReturn {
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

fn touch(_: &mut Interpreter, _: Vec<Value>) -> FunctionReturn {
    Ok(None)
}

fn exit(interp: &mut Interpreter, args: Vec<Value>) -> FunctionReturn {
    if let [value] = &args[..] {
        interp.execute(&OP::PushImmediate(value.clone()), Default::default())?;
        interp.execute(&OP::Yield, Default::default())?;
        interp.exit();
        Ok(None)
    } else {
        Err(Err::new("Exit must be called with one argument"))
    }
}
