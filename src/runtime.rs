use crate::ast::NodeType;
use crate::bytecode::Value;

pub type RuntimeFunction = &'static dyn Fn(&mut Vec<Value>) -> Option<Value>;

pub struct Runtime {
    pub functions: Vec<RuntimeFunctionDefinition>,
}

pub struct RuntimeFunctionDefinition {
    pub name: String,
    pub arguments: Vec<NodeType>,
    pub returns: NodeType,
    pub function: RuntimeFunction,
}

pub fn get() -> Runtime {
    use crate::ast::NodeType::*;
    let functions = vec![
        ff("log", vec![VarArg(Box::new(Any))], Void, &log),
        ff("assert", vec![Any, Any], Void, &assert),
    ];
    Runtime { functions }
}

fn ff(
    name: &str,
    arguments: Vec<NodeType>,
    returns: NodeType,
    function: RuntimeFunction,
) -> RuntimeFunctionDefinition {
    RuntimeFunctionDefinition {
        name: name.to_string(),
        arguments,
        returns,
        function,
    }
}

fn assert(args: &mut Vec<Value>) -> Option<Value> {
    if args.len() != 2 {
        panic!("Assert must be called with two arguments")
    }
    if args[0] != args[1] {
        panic!("assertion failed: {:?} != {:?}", args[0], args[1])
    }
    None
}

fn log(args: &mut Vec<Value>) -> Option<Value> {
    let last_arg = args.pop();
    for arg in args {
        print!("{:?}, ", arg);
    }
    if let Some(arg) = last_arg {
        print!("{:?}", arg);
    }
    println!();
    None
}
