use std::collections::HashMap;

use crate::vm::ast;
use crate::{vm, Err, Path, SubPath};

pub type FunctionReturn = crate::Result<Option<vm::Value>>;
pub type Function = &'static dyn Fn(&mut vm::InterpreterState, Vec<vm::Value>) -> FunctionReturn;

#[derive(Clone)]
pub struct RuntimeDefinitions {
    pub namespace: Namespace,
    pub paths: Vec<Path>,
}

pub struct Runtime {
    pub definitions: RuntimeDefinitions,
    pub functions: Vec<Function>,
}

impl Runtime {
    pub fn add_fn(&mut self, path: Path, mut def: FunctionDefinition, f: Function) {
        let id = self.functions.len();
        def.id = id;

        let el = NamespaceElement::BuiltIn(BuiltInDefinition::Fn(def));
        self.definitions.namespace.set(path.as_ref(), el);
        self.definitions.paths.push(path);
        self.functions.push(f);
    }

    pub fn add_const(&mut self, path: Path, def: ConstDefinition) {
        let el = NamespaceElement::BuiltIn(BuiltInDefinition::Const(def));
        self.definitions.namespace.set(path.as_ref(), el);
    }
}

#[derive(Debug, Clone)]
pub struct FunctionDefinition {
    pub side_effect: bool,
    pub arguments: Vec<ast::NodeType>,
    pub returns: ast::NodeType,
    pub tp: ast::NodeType,
    pub id: usize,
}

#[derive(Debug, Clone)]
pub struct ConstDefinition {
    pub tp: ast::NodeType,
    pub val: ast::NodeValue<()>,
}

#[derive(Debug, Clone)]
pub enum BuiltInDefinition {
    Fn(FunctionDefinition),
    Const(ConstDefinition),
}

impl BuiltInDefinition {
    pub fn body<T>(&self) -> ast::NodeBody<T> {
        match self {
            BuiltInDefinition::Fn(def) => ast::NodeBody::ConstValue {
                tp: None,
                value: ast::NodeValue::RuntimeFn(def.id).into(),
            },
            BuiltInDefinition::Const(_) => unimplemented!(),
        }
    }

    pub fn tp(&self) -> &ast::NodeType {
        match self {
            BuiltInDefinition::Fn(def) => &def.tp,
            BuiltInDefinition::Const(_) => unimplemented!(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum NamespaceElement {
    Namespace(Namespace),
    BuiltIn(BuiltInDefinition),
    Export(AstExport),
}

#[derive(Debug, Copy, Clone)]
pub struct AstExport {
    pub node_id: ast::NodeID,
    pub is_static: bool,
}

#[derive(Debug, Clone)]
pub struct Namespace {
    pub items: HashMap<String, NamespaceElement>,
}

impl Namespace {
    pub fn new() -> Self {
        Self {
            items: HashMap::new(),
        }
    }

    pub fn set(&mut self, path: SubPath, el: NamespaceElement) {
        let child = self.items.get_mut(path.first());
        match child {
            Some(NamespaceElement::Namespace(ref mut n)) => match path.not_first() {
                Some(rest) => n.set(rest, el),
                None => unimplemented!(),
            },
            Some(NamespaceElement::BuiltIn(_)) => unimplemented!(),
            Some(NamespaceElement::Export(_)) => unimplemented!(),
            None => {
                let key = path.first().to_string();
                match path.not_first() {
                    Some(rest) => {
                        let mut namespace = Namespace::new();
                        namespace.set(rest, el);
                        self.items
                            .insert(key, NamespaceElement::Namespace(namespace));
                    }
                    None => {
                        self.items.insert(key, el);
                    }
                }
            }
        }
    }

    pub fn get_tp(&self, path: SubPath) -> Option<&ast::NodeType> {
        use NamespaceElement::*;
        match self.get(path) {
            Some(BuiltIn(def)) => Some(def.tp()),
            _ => None,
        }
    }

    pub fn get(&self, path: SubPath) -> Option<&NamespaceElement> {
        use NamespaceElement::*;
        let child = self.items.get(path.first());
        match child {
            Some(Namespace(n)) => match path.not_first() {
                Some(rest) => n.get(rest),
                None => child,
            },
            Some(BuiltIn(_) | Export(_)) => match path.not_first() {
                None => child,
                Some(_) => None,
            },
            None => None,
        }
    }
}

macro_rules! func {
    ($rt:ident ; $effect:expr; $f:ident as [ $($name:expr),* ] => ( $($args:expr),* ) => $ret:path) => {
        $rt.add_fn(Path::try_new(vec![
            $($name.to_string(),)*
        ]).unwrap(), FunctionDefinition {
            side_effect: $effect,
            arguments: vec![ $($args),* ],
            returns: $ret,
            id: 0,
            tp: ast::NodeType::Fn {
                args: vec![$($args),*],
                returns: Box::new($ret),
            }
        }, &$f);
    };
    ( $rt:ident effect $f:ident as [ $($name:expr),* ] => ( $($args:expr),* ) => $ret:path) => {
        func!($rt ;true; $f as [ $($name),* ] => ( $($args),* ) => $ret)
    };

    ( $rt:ident $f:ident as [ $($name:expr),* ] => ( $($args:expr),* ) => $ret:path) => {
        func!($rt ;false; $f as [ $($name),* ] => ( $($args),* ) => $ret)
    };

    ( $rt:ident effect $f:ident as [ $($name:expr),* ] => ( $($args:expr),* ) ) => {
        func!($rt effect $f as [ $($name),* ] => ( $($args),* ) => Void)
    };

    ( $rt:ident $f:ident as [ $($name:expr),* ] => ( $($args:expr),* ) ) => {
        func!($rt $f as [ $($name),* ] => ( $($args),* ) => Void)
    };
}

fn var(args: ast::NodeType) -> ast::NodeType {
    ast::NodeType::VarArg {
        args: Box::new(args),
    }
}

impl Default for Runtime {
    fn default() -> Self {
        use vm::ast::NodeType::*;
        let mut def = Runtime {
            definitions: RuntimeDefinitions {
                namespace: Namespace::new(),
                paths: vec![],
            },
            functions: vec![],
        };

        func!(def to_int as ["std", "int"] => (Any) => Int);
        func!(def to_float as ["std", "float"] => (Any) => Float);
        func!(def effect print as ["std", "print"] => (Any, var(Any)) => Float);
        func!(def effect assert as ["std", "assert"] => (Any, Any));
        func!(def effect exit as ["std", "exit"] => (Any));
        func!(def effect touch as ["std", "touch"] => (Any));

        func!(def sin as ["math", "sin"] => (Int) => Int);

        def
    }
}

fn to_int(_: &mut vm::InterpreterState, args: Vec<vm::Value>) -> FunctionReturn {
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

fn to_float(_: &mut vm::InterpreterState, args: Vec<vm::Value>) -> FunctionReturn {
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

fn sin(_: &mut vm::InterpreterState, args: Vec<vm::Value>) -> FunctionReturn {
    if args.len() != 1 {
        panic!("Assert must be called with one arguments")
    }
    let result = match args[0] {
        vm::Value::Int(angle) => (angle as f64).sin(),
        _ => panic!("invalid value type passed to sin: {:?}", args[0]),
    };
    Ok(Some(vm::Value::Int(result.round() as isize)))
}

fn assert(_: &mut vm::InterpreterState, args: Vec<vm::Value>) -> FunctionReturn {
    if args.len() != 2 {
        panic!("Assert must be called with two arguments")
    }
    if args[0] != args[1] {
        panic!("assertion failed: {:?} != {:?}", args[0], args[1])
    }
    Ok(None)
}

fn print(_: &mut vm::InterpreterState, mut args: Vec<vm::Value>) -> FunctionReturn {
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

fn touch(_: &mut vm::InterpreterState, _: Vec<vm::Value>) -> FunctionReturn {
    Ok(None)
}

fn exit(interp: &mut vm::InterpreterState, args: Vec<vm::Value>) -> FunctionReturn {
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
