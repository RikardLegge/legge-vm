use ast::{Ast, AstNode};
use token::ArithmeticOp;
use std::collections::HashMap;
use foreign_functions::ForeignFunction;
use std::mem;

#[derive(Debug)]
pub struct Bytecode {
    pub code: Vec<Instruction>
}

impl Bytecode {
    pub fn from_ast(ast: &Ast, foreign_functions: &[ForeignFunction]) -> Self {
        let mut bc = BytecodeGenerator::new(foreign_functions);
        bc.traverse_node(&ast.root);
        bc.get_bytecode()
    }
}

#[derive(Debug)]
pub enum Instruction {
    AddI,
    SubI,
    MulI,
    DivI,

    SLoad(usize),
    SStore(usize),

    PushImmediate(i64),
    Pop,

    CallForeign(usize)
}

#[derive(Copy, Clone)]
struct Address {
    addr: usize,
    kind: AddressKind
}

#[derive(Copy, Clone, PartialEq)]
enum AddressKind {
    ForeignFunction,
    Function,
    StackValue
}

struct Scope {
    parent: Option<Box<Scope>>,
    variables: HashMap<String, Address>,
    allocations: usize,
    code: Vec<Instruction>
}

impl Scope {
    fn new() -> Self {
        Scope {
            parent: None,
            variables: HashMap::new(),
            allocations: 0,
            code: Vec::new()
        }
    }
}

struct BytecodeGenerator {
    code: Vec<Instruction>,
    scope: Scope,
}

impl BytecodeGenerator {
    fn get_bytecode(self) -> Bytecode {
        Bytecode {code: self.code}
    }

    fn new(foreign_functions: &[ForeignFunction]) -> Self {
        let scope = Scope::new();
        let code = Vec::new();

        let mut gen = BytecodeGenerator {code, scope };
        gen.add_foreign_functions_to_global_scope(foreign_functions);
        gen
    }

    fn push_instruction(&mut self, instruction: Instruction) {
        self.scope.code.push(instruction);
    }

    fn add_foreign_functions_to_global_scope(&mut self, foreign_functions: &[ForeignFunction]) {
        for (addr, function) in foreign_functions.iter().enumerate() {
            let name = function.name.to_string();
            let address = Address{addr, kind: AddressKind::ForeignFunction};
            self.scope.variables.insert(name, address);
        }
    }

    fn find_address_in_scope(&self, symbol: &str, scope: &Scope) -> Option<Address> {
        if let Some(val) = scope.variables.get(symbol) {
            Some(*val)
        } else if let Some(parent) = &scope.parent {
            self.find_address_in_scope(symbol, &*parent)
        } else {
            None
        }
    }

    fn find_address(&self, symbol: &str) -> Option<Address> {
        self.find_address_in_scope(symbol, &self.scope)
    }

    fn traverse_node(&mut self, node: &AstNode) {
        use ::ast::AstNode::*;
        match node {
            Op(op, expr1, expr2) => self.ev_operation(*op, &expr1, &expr2),
            PrefixOp(op, expr1) => self.ev_prefix_operation(*op, &expr1),
            Scope(children) => self.ev_scope(children),
            Call(name, args) => self.ev_call(&name, args),
            StaticDeclaration(name, expr) => self.ev_declaration(&name, expr),
            Declaration(name, expr) => self.ev_declaration(&name, expr),
            GetVariable(name) => self.ev_variable_value(&name),
            _ => panic!("Unsupported node here {:?}", node)
        }
    }

    fn ev_variable_value(&mut self, symbol: &str) {
        if let Some(address) = self.find_address(symbol) {
            if address.kind == AddressKind::StackValue {
                let offset = address.addr;
                self.push_instruction(Instruction::SLoad(offset));
            } else {
                unimplemented!();
            }
        } else {
            panic!("Could not find symbol {}", symbol);
        }
    }

    fn ev_declaration(&mut self, symbol: &str, expr: &AstNode) {
        use ::ast::AstNode::*;

        let offset = self.scope.allocations;
        self.scope.allocations += 1;

        self.scope.variables.insert(symbol.to_string(), Address {addr: offset, kind: AddressKind::StackValue });

        match expr {
            Primitive(val) => self.push_instruction(Instruction::PushImmediate(*val)),
            _ => self.traverse_node(expr)
        }

        self.push_instruction(Instruction::SStore(offset));
    }


    fn ev_call(&mut self, symbol: &str, args: &[AstNode]) {
        let address = self.find_address(symbol);
        if address.is_none() {panic!("Symbol not found in the current scope");}
        let address = address.unwrap();

        match address.kind {
            AddressKind::ForeignFunction => {
                for arg in args  {
                    self.traverse_node(arg);
                }
                self.push_instruction(Instruction::PushImmediate(args.len() as i64));
                self.push_instruction(Instruction::CallForeign(address.addr));
            },
            _ => unimplemented!()
        }
    }

    fn ev_scope(&mut self, children: &Vec<AstNode>) {
        let parent = mem::replace(&mut self.scope, Scope::new());
        self.scope.parent = Some(Box::new(parent));

        for node in children {
            self.traverse_node(node);
        }

        let parent = *mem::replace(&mut self.scope.parent, None).unwrap();
        let scope = mem::replace(&mut self.scope, parent);

        for _ in 0..scope.allocations {
            self.code.push(Instruction::PushImmediate(0));
        }

        for instruction in scope.code {
            self.code.push(instruction)
        }

        for _ in 0..scope.allocations {
            self.code.push(Instruction::Pop);
        }
    }

    fn ev_prefix_operation(&mut self, op: ArithmeticOp, expr1: &AstNode) {
        self.push_instruction(Instruction::PushImmediate(0));
        self.ev_expression(expr1);
        match op {
            ArithmeticOp::Add => self.push_instruction(Instruction::AddI),
            ArithmeticOp::Sub => self.push_instruction(Instruction::SubI),
            _ => panic!("Invalid prefix operation: {:?}", op)
        }
    }

    fn ev_operation(&mut self, op: ArithmeticOp, expr1: &AstNode, expr2: &AstNode) {
        self.ev_expression(expr1);
        self.ev_expression(expr2);
        match op {
            ArithmeticOp::Add => self.push_instruction(Instruction::AddI),
            ArithmeticOp::Sub => self.push_instruction(Instruction::SubI),
            ArithmeticOp::Mul => self.push_instruction(Instruction::MulI),
            ArithmeticOp::Div => self.push_instruction(Instruction::DivI)
        }
    }

    fn ev_expression(&mut self, expr: &AstNode) {
        use ::ast::AstNode::*;
        match expr {
            Op(op, expr1, expr2) => self.ev_operation(*op, &expr1, &expr2),
            Primitive(primitive) => self.push_instruction(Instruction::PushImmediate(primitive.clone())),
            GetVariable(symbol) => self.ev_variable_value(symbol),
            _ => panic!("Unsupported node here")
        }
    }

}
