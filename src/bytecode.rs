use ast::{Ast, AstNode};
use token::ArithmeticOp;
use std::collections::HashMap;
use foreign_functions::ForeignFunction;
use std::mem;
use std::ops::AddAssign;

#[derive(Debug)]
pub struct Bytecode {
    pub procedure_address: usize,
    pub code: Vec<Instruction>,
    pub data: Vec<i64>,
}

impl Bytecode {
    pub fn from_ast(ast: &Ast, foreign_functions: &[ForeignFunction]) -> Self {
        let mut bc = BytecodeGenerator::new(foreign_functions);
        assert_eq!(StackUsage { pushed: 0, popped: 0 }, bc.ev_node(&ast.root));
        bc.get_bytecode()
    }
}

#[derive(Debug, PartialEq)]
pub enum Instruction {
    AddI,
    SubI,
    MulI,
    DivI,

    SLoad(usize),
    SStore(usize),

    PushPc(usize),
    PopPc,

    PushImmediate(i64),
    Pop,

    SetFrame(i64),
    PushFrame,
    PopFrame,

    Call(usize),
    CallForeign(usize),

    Halt,
}

#[derive(Copy, Clone, Debug)]
struct Address {
    addr: usize,
    kind: AddressKind,
}

#[derive(Copy, Clone, PartialEq, Debug)]
enum AddressKind {
    ForeignFunction,
    Function,
    StackValue,
    ConstStackValue,
    HeapValue,
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
    pushed: usize,
}

impl AddAssign for StackUsage {
    fn add_assign(&mut self, rhs: Self) {
        self.pushed += rhs.pushed;
        self.popped += rhs.popped;
    }
}

impl StackUsage {
    fn new(popped: usize, pushed: usize) -> Self {
        StackUsage { popped, pushed }
    }
}

struct BytecodeGenerator<'a> {
    code: Vec<Instruction>,
    heap: Vec<i64>,
    scope: Scope,
    procedures: Vec<Instruction>,
    foreign_functions: &'a [ForeignFunction],
}

impl<'a> BytecodeGenerator<'a> {
    fn get_bytecode(self) -> Bytecode {
        let data = self.heap;

        let mut code = self.code;
        code.push(Instruction::Halt);
        let procedure_address = code.len();

        let mut procedures = self.procedures;
        code.append(&mut procedures);

        Bytecode { code, data, procedure_address }
    }

    fn new(foreign_functions: &'a [ForeignFunction]) -> Self {
        let scope = Scope::new();
        let code = Vec::new();
        let heap = Vec::new();
        let procedures = Vec::new();

        let mut gen = BytecodeGenerator { code, scope, foreign_functions, heap, procedures };
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

    fn get_stack_allocation_offset(&self) -> usize {
        let mut offset = self.scope.allocations;
        let mut parent = &self.scope.parent;
        while let Some(scope) = parent {
            offset += scope.allocations;
            parent = &scope.parent;
        };
        offset
    }

    fn ev_node(&mut self, node: &AstNode) -> StackUsage {
        use ::ast::AstNode::*;
        match node {
            Op(op, expr1, expr2) => self.ev_operation(*op, &expr1, &expr2),
            PrefixOp(op, expr1) => self.ev_prefix_operation(*op, &expr1),
            Scope(children) => self.ev_scope(children),
            Call(name, args) => self.ev_call(&name, args),
            ConstDeclaration(name, expr) => self.ev_declaration(&name, expr, true),
            Declaration(name, expr) => self.ev_declaration(&name, expr, false),
            GetVariable(name) => self.ev_variable_value(&name),
            String(value) => self.ev_string(&value),
            Assignment(name, expr) => self.ev_assignment(&name, expr, false),
            ProcedureDeclaration(name, args, body) => self.ev_procedure(name, &args, body),
            _ => panic!("Unsupported node here {:?}", node)
        }
    }

    fn ev_procedure(&mut self, symbol: &str, args: &[String], body: &[AstNode]) -> StackUsage {
        let proc_address = self.procedures.len();

        {
            self.push_scope();
            for arg in args {
                let offset = self.get_stack_allocation_offset();
                self.scope.allocations += 1;
                self.scope.variables.insert(arg.to_string(), Address { addr: offset, kind: AddressKind::StackValue });
            }

            {
                self.push_scope();

                let mut stack_usage = StackUsage::new(0, 0);
                for node in body {
                    stack_usage += self.ev_node(node);
                }
                assert_eq!(stack_usage.popped, stack_usage.pushed);

                let scope = self.pop_scope();

                for _ in 0..scope.allocations {
                    self.procedures.push(Instruction::PushImmediate(0));
                }

                for instruction in scope.code {
                    self.procedures.push(instruction)
                }

                for _ in 0..scope.allocations {
                    self.procedures.push(Instruction::Pop);
                }
            }

            let scope = self.pop_scope();
            for _ in 0..scope.allocations {
                self.procedures.push(Instruction::Pop);
            }

            self.procedures.push(Instruction::PopFrame);
            self.procedures.push(Instruction::PopPc);
        }

        self.scope.variables.insert(symbol.to_string(), Address {addr: proc_address, kind: AddressKind::Function});

        StackUsage::new(0, 0)
    }

    fn ev_string(&mut self, string: &str) -> StackUsage {
        let address = self.heap.len();
        let bytes = string.as_bytes();

        for byte in bytes {
            self.heap.push(*byte as i64);
        }
        self.push_instruction(Instruction::PushImmediate(address as i64));
        StackUsage::new(0, 1)
    }

    fn ev_variable_value(&mut self, symbol: &str) -> StackUsage {
        use self::AddressKind::*;
        let address = self.find_address(symbol).expect("Symbol not found in the current scope");
        match address.kind {
            StackValue | ConstStackValue => {
                let offset = address.addr;
                self.push_instruction(Instruction::SLoad(offset));
                StackUsage::new(0, 1)
            }
            _ => panic!("{:?}", address)
        }
    }

    fn ev_assignment(&mut self, symbol: &str, expr: &AstNode, is_declaration: bool) -> StackUsage {
        use ::ast::AstNode::*;

        let address = self.find_address(symbol).expect("Symbol not found in the current scope");
        if address.kind == AddressKind::ConstStackValue && !is_declaration {
            panic!("Can not assign to constant value");
        }

        let stack_usage = match expr {
            Primitive(val) => self.ev_intermediate(*val),
            _ => self.ev_node(expr)
        };
        assert_eq!(1, stack_usage.pushed);

        self.push_instruction(Instruction::SStore(address.addr));
        StackUsage::new(stack_usage.popped, 0)
    }

    fn ev_declaration(&mut self, symbol: &str, expr: &AstNode, is_constant: bool) -> StackUsage {
        let offset = self.get_stack_allocation_offset();
        self.scope.allocations += 1;
        if is_constant {
            self.scope.variables.insert(symbol.to_string(), Address { addr: offset, kind: AddressKind::ConstStackValue });
        } else {
            self.scope.variables.insert(symbol.to_string(), Address { addr: offset, kind: AddressKind::StackValue });
        }

        self.ev_assignment(symbol, expr, true)
    }


    fn ev_call(&mut self, symbol: &str, args: &[AstNode]) -> StackUsage {
        let address = self.find_address(symbol).expect("Symbol not found in the current scope");

        match address.kind {
            AddressKind::ForeignFunction => {
                let mut popped = 0;
                for arg in args {
                    let usage = self.ev_expression(arg);
                    assert_eq!(1, usage.pushed);
                    popped += usage.popped;
                }
                self.push_instruction(Instruction::PushImmediate(args.len() as i64));
                self.push_instruction(Instruction::CallForeign(address.addr));
                let pushed = self.foreign_functions[address.addr].returns;
                StackUsage::new(popped, pushed)
            }
            AddressKind::Function => {
                let call_instruction_count = 3;
                self.push_instruction(Instruction::PushPc(args.len() + call_instruction_count));
                self.push_instruction(Instruction::PushFrame);
                let mut popped = 0;
                for arg in args {
                    let usage = self.ev_expression(arg);
                    assert_eq!(1, usage.pushed);
                    popped += usage.popped;
                }
                self.push_instruction(Instruction::SetFrame(-(args.len() as i64)));
                self.push_instruction(Instruction::Call(address.addr));
                StackUsage::new(popped, 0)
            }
            _ => unimplemented!()
        }
    }

    fn push_scope(&mut self) {
        let parent = mem::replace(&mut self.scope, Scope::new());
        self.scope.parent = Some(Box::new(parent));
    }

    fn pop_scope(&mut self) -> Scope {
        let parent = *mem::replace(&mut self.scope.parent, None).unwrap();
        mem::replace(&mut self.scope, parent)
    }

    fn ev_scope(&mut self, children: &Vec<AstNode>) -> StackUsage {
        self.push_scope();

        let mut stack_usage = StackUsage::new(0, 0);
        for node in children {
            stack_usage += self.ev_node(node);
        }
        assert_eq!(stack_usage.popped, stack_usage.pushed);

        let scope = self.pop_scope();

        for _ in 0..scope.allocations {
            self.code.push(Instruction::PushImmediate(0));
        }

        for instruction in scope.code {
            self.code.push(instruction)
        }

        for _ in 0..scope.allocations {
            self.code.push(Instruction::Pop);
        }

        StackUsage::new(0, 0)
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
        StackUsage::new(diff.popped, 1)
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
        StackUsage::new(diff1.popped + diff2.popped, 1)
    }

    fn ev_expression(&mut self, expr: &AstNode) -> StackUsage {
        use ::ast::AstNode::*;
        match expr {
            Op(op, expr1, expr2) => self.ev_operation(*op, &expr1, &expr2),
            Primitive(primitive) => self.ev_intermediate(*primitive),
            GetVariable(symbol) => self.ev_variable_value(symbol),
            _ => panic!("Unsupported node here {:?}", expr)
        }
    }

    fn ev_intermediate(&mut self, value: i64) -> StackUsage {
        self.push_instruction(Instruction::PushImmediate(value));
        StackUsage::new(0, 1)
    }
}
