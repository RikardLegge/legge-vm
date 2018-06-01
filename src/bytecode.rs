use ast::{Ast, AstNode};
use token::ArithmeticOp;
use std::collections::HashMap;
use foreign_functions::ForeignFunction;
use std::mem;
use std::ops::AddAssign;

#[derive(Debug)]
pub struct Bytecode {
    pub code: Vec<Instruction>
}

impl Bytecode {
    pub fn from_ast(ast: &Ast, foreign_functions: &[ForeignFunction]) -> Self {
        let mut bc = BytecodeGenerator::new(foreign_functions);
        assert_eq!(StackUsage {pushed: 0, popped: 0}, bc.ev_node(&ast.root));
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

    CallForeign(usize),
}

#[derive(Copy, Clone)]
struct Address {
    addr: usize,
    kind: AddressKind,
}

#[derive(Copy, Clone, PartialEq)]
enum AddressKind {
    ForeignFunction,
    Function,
    StackValue,
}

struct Scope {
    parent: Option<Box<Scope>>,
    variables: HashMap<String, Address>,
    allocations: usize,
    code: Vec<Instruction>,
}

impl Scope {
    fn new() -> Self {
        Scope {
            parent: None,
            variables: HashMap::new(),
            allocations: 0,
            code: Vec::new(),
        }
    }
}

#[derive(PartialEq, Debug)]
struct StackUsage {
    popped: usize,
    pushed: usize
}

impl AddAssign for StackUsage {
    fn add_assign(&mut self, rhs: Self) {
        self.pushed += rhs.pushed;
        self.popped += rhs.popped;
    }
}

impl StackUsage {
    fn new(popped: usize, pushed: usize) -> Self {
        StackUsage {popped, pushed}
    }
}

struct BytecodeGenerator<'a> {
    code: Vec<Instruction>,
    scope: Scope,
    foreign_functions: &'a [ForeignFunction],
}

impl<'a> BytecodeGenerator<'a> {
    fn get_bytecode(self) -> Bytecode {
        Bytecode { code: self.code }
    }

    fn new(foreign_functions: &'a [ForeignFunction]) -> Self {
        let scope = Scope::new();
        let code = Vec::new();

        let mut gen = BytecodeGenerator { code, scope, foreign_functions };
        for (addr, function) in foreign_functions.iter().enumerate() {
            let name = function.name.to_string();
            let address = Address { addr, kind: AddressKind::ForeignFunction };
            gen.scope.variables.insert(name, address);
        }
        gen
    }

    fn push_instruction(&mut self, instruction: Instruction) {
        self.scope.code.push(instruction);
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

    fn ev_node(&mut self, node: &AstNode) -> StackUsage {
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

    fn ev_variable_value(&mut self, symbol: &str) -> StackUsage {
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
        StackUsage::new(0,1)
    }

    fn ev_declaration(&mut self, symbol: &str, expr: &AstNode) -> StackUsage {
        use ::ast::AstNode::*;

        let offset = self.scope.allocations;
        self.scope.allocations += 1;

        self.scope.variables.insert(symbol.to_string(), Address { addr: offset, kind: AddressKind::StackValue });

        let mut stack_usage = match expr {
            Primitive(val) => self.ev_intermediate(*val),
            _ => self.ev_node(expr)
        };
        assert_eq!(1, stack_usage.pushed);

        self.push_instruction(Instruction::SStore(offset));
        StackUsage::new(stack_usage.popped, 0)
    }


    fn ev_call(&mut self, symbol: &str, args: &[AstNode]) -> StackUsage {
        let address = self.find_address(symbol);
        if address.is_none() { panic!("Symbol not found in the current scope"); }
        let address = address.unwrap();

        let popped = match address.kind {
            AddressKind::ForeignFunction => {
                let mut popped = 0;
                for arg in args {
                    let usage = self.ev_node(arg);
                    assert_eq!(1, usage.pushed);
                    popped += usage.popped;
                }
                self.push_instruction(Instruction::PushImmediate(args.len() as i64));
                self.push_instruction(Instruction::CallForeign(address.addr));
                popped
            }
            _ => unimplemented!()
        };
        let pushed = self.foreign_functions[address.addr].returns;
        StackUsage::new(popped,pushed)

    }

    fn ev_scope(&mut self, children: &Vec<AstNode>) -> StackUsage {
        let parent = mem::replace(&mut self.scope, Scope::new());
        self.scope.parent = Some(Box::new(parent));

        let mut stack_usage = StackUsage::new(0,0);
        for node in children {
             stack_usage += self.ev_node(node);
        }
        assert_eq!(stack_usage.popped, stack_usage.pushed);

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

        StackUsage::new(0,0)
    }

    fn ev_prefix_operation(&mut self, op: ArithmeticOp, expr1: &AstNode) -> StackUsage {
        self.push_instruction(Instruction::PushImmediate(0));
        let diff = self.ev_expression(expr1);
        assert_eq!(1, diff.pushed);
        match op {
            ArithmeticOp::Add => self.push_instruction(Instruction::AddI),
            ArithmeticOp::Sub => self.push_instruction(Instruction::SubI),
            _ => panic!("Invalid prefix operation: {:?}", op)
        };
        StackUsage::new(diff.popped,1)
    }

    fn ev_operation(&mut self, op: ArithmeticOp, expr1: &AstNode, expr2: &AstNode) -> StackUsage {
        let diff1 = self.ev_expression(expr1);
        let diff2 = self.ev_expression(expr2);
        assert_eq!(1, diff1.pushed);
        assert_eq!(1, diff2.pushed);

        match op {
            ArithmeticOp::Add => self.push_instruction(Instruction::AddI),
            ArithmeticOp::Sub => self.push_instruction(Instruction::SubI),
            ArithmeticOp::Mul => self.push_instruction(Instruction::MulI),
            ArithmeticOp::Div => self.push_instruction(Instruction::DivI)
        };
        StackUsage::new(diff1.popped + diff2.popped,1)
    }

    fn ev_expression(&mut self, expr: &AstNode) -> StackUsage {
        use ::ast::AstNode::*;
        match expr {
            Op(op, expr1, expr2) => self.ev_operation(*op, &expr1, &expr2),
            Primitive(primitive) => self.ev_intermediate(*primitive),
            GetVariable(symbol) => self.ev_variable_value(symbol),
            _ => panic!("Unsupported node here")
        }
    }

    fn ev_intermediate(&mut self, value: i64) -> StackUsage {
        self.push_instruction(Instruction::PushImmediate(value));
        StackUsage::new(0,1)
    }
}
