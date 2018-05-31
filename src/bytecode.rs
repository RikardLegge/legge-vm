use ast::{Ast, AstNode};
use token::ArithmeticOp;
use std::collections::HashMap;
use sdl::log;
use interpreter::Interpreter;
use interpreter::InterpResult;

pub struct Bytecode {
    pub code: Vec<Instruction>,
    pub data: Vec<Data>
}

impl Bytecode {
    pub fn from_ast(ast: &Ast) -> Self {
        let mut bc = BytecodeGenerator::new();
        bc.traverse_node(&ast.root);
        bc.get_bytecode()
    }
}

pub struct FFIFunction {
    pub arguments: i32,
    pub returns: i32,
    pub function: &'static Fn(&mut Interpreter, &Bytecode) -> InterpResult
}

pub enum Data {
    FFiFunction(usize, FFIFunction),
    Constant(usize, i64)
}

#[derive(Debug)]
pub enum Instruction {
    AddI,
    SubI,
    MulI,
    DivI,
    PushImmediate(i64),
    PushLoad(usize),
    BlFFI(usize)
}

struct Scope {
    parent: Option<Box<Scope>>,
    data: HashMap<String, usize>
}

struct BytecodeGenerator {
    code: Vec<Instruction>,
    data: Vec<Data>,
    scope: Scope
}

impl BytecodeGenerator {
    fn get_bytecode(self) -> Bytecode {
        let code = self.code;
        let data = self.data;
        Bytecode {code, data}
    }

    fn new() -> Self {
        let scope = Scope {parent: None, data: HashMap::new()};
        let code = Vec::new();
        let data = Vec::new();

        let mut gen = BytecodeGenerator {code, scope, data};
        gen.add_external_function("log", FFIFunction {arguments: -1, returns: 0, function: &log});
        gen
    }

    fn add_external_function(&mut self, name: &str, func: FFIFunction) {
        let id = self.data.len();
        let data = Data::FFiFunction(id, func);
        self.data.push(data);
        self.scope.data.insert(name.to_string(), id);
    }

    fn find_address_in_scope(&self, symbol: &str, scope: &Scope) -> Option<usize> {
        if let Some(val) = scope.data.get(symbol) {
            Some(*val)
        } else if let Some(parent) = &scope.parent {
            self.find_address_in_scope(symbol, &*parent)
        } else {
            None
        }
    }

    fn find_address(&self, symbol: &str) -> Option<usize> {
        self.find_address_in_scope(symbol, &self.scope)
    }

    fn traverse_node(&mut self, node: &AstNode) {
        use ::ast::AstNode::*;
        match node {
            Op(op, expr1, expr2) => self.ev_operation(*op, &expr1, &expr2),
            PrefixOp(op, expr1) => self.ev_prefix_operation(*op, &expr1),
            Scope(children) => self.ev_scope(children),
            Call(name, args) => self.ev_call(&name, args),
            Declaration(name, expr) => self.ev_declaration(&name, expr),
            GetVariable(name) => self.ev_variable_value(&name),
            _ => panic!("Unsupported node here {:?}", node)
        }
    }

    fn ev_variable_value(&mut self, symbol: &str) {
        if let Some(addr) = self.find_address(symbol) {
            self.code.push(Instruction::PushLoad(addr));
        } else {
            panic!("Could not find symbol {}", symbol)
        }
    }

    fn ev_declaration(&mut self, symbol: &str, expr: &AstNode) {
        use ::ast::AstNode::*;

        let addr = self.data.len();
        match expr {
            Primitive(val) => self.data.push(Data::Constant(addr, *val)),
            _ => self.traverse_node(expr)
        }
        self.scope.data.insert(symbol.to_string(), addr);
    }

    fn ev_call(&mut self, symbol: &str, args: &[AstNode]) {
        let addr = self.find_address(symbol);
        if addr.is_none() {panic!("Symbol not found in the current scope");}
        let addr = addr.unwrap();

        let data = self.data.get(addr);
        if data.is_none() {panic!("Symbol found but the address was invalid");}
        let data = data.unwrap();

        match data {
            Data::FFiFunction(id, _) => {
                let id = *id;
                for arg in args  {
                    self.traverse_node(arg);
                }
                self.code.push(Instruction::PushImmediate(args.len() as i64));
                self.code.push(Instruction::BlFFI(id));
            },
            _ => panic!("Symbol is not a function")
        }
    }

    fn ev_scope(&mut self, children: &Vec<AstNode>) {
        for node in children {
            self.traverse_node(node);
        }
    }

    fn ev_prefix_operation(&mut self, op: ArithmeticOp, expr1: &AstNode) {
        self.code.push(Instruction::PushImmediate(0));
        self.ev_expression(expr1);
        match op {
            ArithmeticOp::Add => self.code.push(Instruction::AddI),
            ArithmeticOp::Sub => self.code.push(Instruction::SubI),
            _ => panic!("Invalid prefix operation: {:?}", op)
        }
    }

    fn ev_operation(&mut self, op: ArithmeticOp, expr1: &AstNode, expr2: &AstNode) {
        self.ev_expression(expr1);
        self.ev_expression(expr2);
        match op {
            ArithmeticOp::Add => self.code.push(Instruction::AddI),
            ArithmeticOp::Sub => self.code.push(Instruction::SubI),
            ArithmeticOp::Mul => self.code.push(Instruction::MulI),
            ArithmeticOp::Div => self.code.push(Instruction::DivI)
        }
    }

    fn ev_expression(&mut self, expr: &AstNode) {
        use ::ast::AstNode::*;
        match expr {
            Op(op, expr1, expr2) => self.ev_operation(*op, &expr1, &expr2),
            Primitive(primitive) => self.code.push(Instruction::PushImmediate(primitive.clone())),
            GetVariable(symbol) => self.ev_variable_value(symbol),
            _ => panic!("Unsupported node here")
        }
    }

}
