use std::sync::Arc;

use crate::vm::ast;
use crate::{vm, Err};

pub type FunctionReturn = crate::Result<Option<vm::Value>>;
pub type Function = &'static dyn Fn(&mut vm::Interpreter, Vec<vm::Value>) -> FunctionReturn;

pub struct Runtime {
    pub definitions: Arc<Vec<FunctionDefinition>>,
    pub functions: Vec<Function>,
}

#[derive(Debug)]
pub struct FunctionDefinition {
    pub module: String,
    pub name: String,
    pub arguments: Vec<ast::NodeType>,
    pub returns: ast::NodeType,
    pub tp: ast::NodeType,
}

impl Default for Runtime {
    fn default() -> Self {
        use vm::ast::NodeType::*;
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
}

fn ff(
    functions: &mut Vec<Function>,
    definitions: &mut Vec<FunctionDefinition>,
    namespace: &str,
    name: &str,
    arguments: Vec<ast::NodeType>,
    returns: ast::NodeType,
    function: Function,
) {
    let tp = ast::NodeType::Fn {
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

fn to_int(_: &mut vm::Interpreter, args: Vec<vm::Value>) -> FunctionReturn {
    if args.len() != 1 {
        panic!("int must be called with one arguments")
    }
    let result = match args[0] {
        vm::Value::Int(int) => vm::Value::Int(int),
        vm::Value::Float(int) => vm::Value::Int(int as isize),
        _ => panic!("invalid value type passed to int: {:?}", args[0]),
    };
    Ok(Some(result))
}

fn to_float(_: &mut vm::Interpreter, args: Vec<vm::Value>) -> FunctionReturn {
    if args.len() != 1 {
        panic!("int must be called with one arguments")
    }
    let result = match args[0] {
        vm::Value::Int(int) => vm::Value::Float(int as f64),
        vm::Value::Float(float) => vm::Value::Float(float),
        _ => panic!("invalid value type passed to int: {:?}", args[0]),
    };
    Ok(Some(result))
}

fn sin(_: &mut vm::Interpreter, args: Vec<vm::Value>) -> FunctionReturn {
    if args.len() != 1 {
        panic!("Assert must be called with one arguments")
    }
    let result = match args[0] {
        vm::Value::Int(angle) => (angle as f64).sin(),
        _ => panic!("invalid value type passed to sin: {:?}", args[0]),
    };
    Ok(Some(vm::Value::Int(result.round() as isize)))
}

fn assert(_: &mut vm::Interpreter, args: Vec<vm::Value>) -> FunctionReturn {
    if args.len() != 2 {
        panic!("Assert must be called with two arguments")
    }
    if args[0] != args[1] {
        panic!("assertion failed: {:?} != {:?}", args[0], args[1])
    }
    Ok(None)
}

fn print(_: &mut vm::Interpreter, mut args: Vec<vm::Value>) -> FunctionReturn {
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

fn touch(_: &mut vm::Interpreter, _: Vec<vm::Value>) -> FunctionReturn {
    Ok(None)
}

fn exit(interp: &mut vm::Interpreter, args: Vec<vm::Value>) -> FunctionReturn {
    if let [value] = &args[..] {
        interp.execute(
            &vm::OPCode::PushImmediate(value.clone()),
            Default::default(),
        )?;
        interp.execute(&vm::OPCode::Yield, Default::default())?;
        interp.exit();
        Ok(None)
    } else {
        Err(Err::new("Exit must be called with one argument"))
    }
}
