use crate::ast::{AssignmentType, Ast, AstNode};
use crate::bytecode::ScopeType::StackFrame;
use crate::foreign_functions::ForeignFunction;
use crate::token::ArithmeticOp;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::mem;
use std::ops::AddAssign;

const CALL_INSTRUCTION_COUNT: usize = 3; // PushPC, PushStackFrame, SetStackFrame

#[derive(Debug, Serialize, Deserialize)]
pub struct Bytecode {
    pub procedure_address: usize,
    pub code: Vec<Instruction>,
    pub data: Vec<i64>,
}

impl Bytecode {
    pub fn from_ast(ast: &Ast, foreign_functions: &[ForeignFunction]) -> Self {
        let mut bc = BytecodeGenerator::new(foreign_functions);
        assert_eq!(
            StackUsage {
                pushed: 0,
                popped: 0
            },
            bc.ev_node(&ast.root)
        );
        let global_scope = mem::replace(&mut bc.scope, Scope::new());
        assert_eq!(bc.code.len(), 0);
        bc.code = global_scope.code;
        bc.optimize();
        bc.get_bytecode()
    }
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum Instruction {
    AddI,
    SubI,
    MulI,
    DivI,
    EqI,

    SLoad(i64),
    SStore(i64),

    Branch(isize),
    BranchIf(isize),
    PushPc(usize),
    PopPc,

    PushImmediate(i64, String),
    PopStack(usize),

    SetStackFrame(i64),
    PushStackFrame,
    PopStackFrame,

    CreateClosure,

    Call(usize, String),
    CallForeign(usize, String),

    NoOp,

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
    Function {
        argument_count: usize,
        return_value: bool,
    },
    StackValue,
    ConstStackValue,
}

#[derive(Copy, Clone, Debug, PartialEq)]
enum ScopeType {
    Scope,
    Loop,
    StackFrame,
}

struct Scope {
    parent: Option<Box<Scope>>,
    variables: HashMap<String, Address>,
    allocations: usize,
    tp: ScopeType,
    code: Vec<Instruction>,
}

impl Scope {
    fn new() -> Self {
        Scope {
            parent: None,
            variables: HashMap::new(),
            allocations: 0,
            code: Vec::new(),
            tp: ScopeType::Scope,
        }
    }

    fn get_parent_allocations(&self, break_on_scope: ScopeType) -> usize {
        if let Some(p) = &self.parent {
            if p.tp != break_on_scope {
                return self.allocations + p.get_parent_allocations(break_on_scope);
            }
        }
        return self.allocations;
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
    fn optimize(&mut self) {
        let mut i = 1;
        while i < self.code.len() {
            match &self.code[i] {
                // Remove consecutive store-load instructions
                // Ex:
                // Inst::SStore(n)
                // Inst::SLoad(n)
                // -> NoOp
                Instruction::SLoad(load_offset) => {
                    if let Instruction::SStore(store_offset) = self.code[i - 1] {
                        if *load_offset == store_offset {
                            self.code[i - 1] = Instruction::NoOp;
                            self.code[i] = Instruction::NoOp;
                        }
                    }
                }
                Instruction::Branch(offset) => {
                    if *offset == 0 {
                        self.code[i] = Instruction::NoOp;
                    }
                }
                _ => {}
            }
            i = i + 1;
        }
    }

    fn get_bytecode(self) -> Bytecode {
        let data = self.heap;

        let mut code = self.code;
        code.push(Instruction::Halt);
        let procedure_address = code.len();

        let mut procedures = self.procedures;
        code.append(&mut procedures);

        Bytecode {
            code,
            data,
            procedure_address,
        }
    }

    fn new(foreign_functions: &'a [ForeignFunction]) -> Self {
        let scope = Scope::new();
        let code = Vec::new();
        let heap = Vec::new();
        let procedures = Vec::new();

        let mut gen = BytecodeGenerator {
            code,
            scope,
            foreign_functions,
            heap,
            procedures,
        };
        for (addr, function) in foreign_functions.iter().enumerate() {
            let name = function.name.to_string();
            let address = Address {
                addr,
                kind: AddressKind::ForeignFunction,
            };
            gen.scope.variables.insert(name, address);
        }
        gen
    }

    fn push_instruction(&mut self, instruction: Instruction) {
        self.scope.code.push(instruction);
    }

    fn find_address_in_scope(
        &self,
        symbol: &str,
        scope: &Scope,
        recurse_frames: bool,
    ) -> Option<Address> {
        if let Some(val) = scope.variables.get(symbol) {
            return Some(*val);
        } else if !(scope.tp == ScopeType::StackFrame) || recurse_frames {
            if let Some(parent) = &scope.parent {
                return self.find_address_in_scope(symbol, &*parent, recurse_frames);
            }
        }

        None
    }

    fn find_fn_address(&self, symbol: &str) -> Option<Address> {
        self.find_address_in_scope(symbol, &self.scope, true)
    }

    fn find_var_address(&self, symbol: &str) -> Option<Address> {
        self.find_address_in_scope(symbol, &self.scope, false)
    }

    fn get_stack_allocation_offset(&self) -> usize {
        let mut offset = self.scope.allocations;
        if !(self.scope.tp == StackFrame) {
            let mut parent = &self.scope.parent;
            while let Some(scope) = parent {
                offset += scope.allocations;
                if scope.tp == StackFrame {
                    parent = &None
                } else {
                    parent = &scope.parent
                }
            }
        }
        offset
    }

    fn push_scope(&mut self, tp: ScopeType) {
        let parent = mem::replace(&mut self.scope, Scope::new());
        self.scope.parent = Some(Box::new(parent));
        self.scope.tp = tp;
    }

    fn pop_scope(&mut self, tp: ScopeType) -> Scope {
        assert_eq!(self.scope.tp, tp);
        self.pop_scope_unsafe()
    }

    fn pop_scope_unsafe(&mut self) -> Scope {
        let parent = *mem::replace(&mut self.scope.parent, None).unwrap();
        mem::replace(&mut self.scope, parent)
    }

    fn ev_node(&mut self, node: &AstNode) -> StackUsage {
        use crate::ast::AstNode::*;
        match node {
            Op(op, expr1, expr2) => self.ev_operation(*op, &expr1, &expr2),
            PrefixOp(op, expr1) => self.ev_prefix_operation(*op, &expr1),
            Scope(children) => self.ev_scope(children),
            Call(name, args) => self.ev_call(&name, args),
            GetVariable(name) => self.ev_variable_value(&name),
            String(value) => self.ev_string(&value),
            Assignment(tp, name, expr) => match tp {
                AssignmentType::Assignment => self.ev_assignment(&name, expr, false),
                AssignmentType::Declaration => self.ev_declaration(&name, expr, false),
                AssignmentType::ConstDeclaration => self.ev_declaration(&name, expr, true),
            },
            ProcedureDeclaration(name, args, return_values, body) => {
                self.ev_procedure(name, &args, &return_values, body)
            }
            Return(return_value) => self.ev_return(return_value),
            If(condition, body) => self.ev_if(condition, body),
            Loop(body) => self.ev_loop(body),
            _ => panic!("Unsupported node here {:?}", node),
        }
    }

    fn ev_loop(&mut self, body: &Box<AstNode>) -> StackUsage {
        self.push_scope(ScopeType::Loop);
        self.scope.tp = ScopeType::Loop;

        let start = self.scope.code.len();
        if let AstNode::Scope(children) = &**body {
            let usage = self.ev_scope(children);
            assert_eq!(usage.pushed, usage.popped);
        } else {
            panic!("The body of a loop statement must be a scope");
        }
        let end = self.scope.code.len();
        let start_of_loop = (start - end) as isize;
        self.scope.code.push(Instruction::Branch(start_of_loop));

        self.pop_scope(ScopeType::Loop);

        StackUsage {
            popped: 0,
            pushed: 0,
        }
    }

    fn ev_if(&mut self, condition: &Box<AstNode>, body: &Box<AstNode>) -> StackUsage {
        let usage = self.ev_expression(condition);
        assert_eq!(usage.pushed - usage.popped, 1);
        self.scope.code.push(Instruction::PushImmediate(
            0,
            "Invert increment condition".into(),
        ));
        self.scope.code.push(Instruction::EqI);
        let jump_index = self.scope.code.len();
        self.scope.code.push(Instruction::NoOp);

        let start = self.scope.code.len();
        if let AstNode::Scope(children) = &**body {
            let usage = self.ev_scope(children);
            assert_eq!(usage.pushed, usage.popped);
        } else {
            panic!("The body of an if statement must be a scope");
        }
        let end = self.scope.code.len();
        let end_of_if = (end - start) as isize;
        self.scope.code[jump_index] = Instruction::BranchIf(end_of_if);

        StackUsage {
            popped: 0,
            pushed: 0,
        }
    }

    fn ev_return(&mut self, return_value: &Option<Box<AstNode>>) -> StackUsage {
        let parent_alloc = self.scope.get_parent_allocations(ScopeType::StackFrame);
        if let Some(val) = return_value {
            let usage = self.ev_expression(val);
            assert_eq!(usage.pushed - usage.popped, 1);
            // Immediately store the value found on the stack
            // the
            let return_value_offset = -(CALL_INSTRUCTION_COUNT as i64) - parent_alloc as i64
                + self.scope.allocations as i64;
            self.scope
                .code
                .push(Instruction::SStore(return_value_offset));
        }

        self.scope.code.push(Instruction::PopStack(parent_alloc));
        self.scope.code.push(Instruction::PopStackFrame);
        self.scope.code.push(Instruction::PopPc);

        StackUsage::new(0, 0)
    }

    fn ev_procedure_scope(&mut self, body: &[AstNode]) {
        use self::Instruction::*;
        let scope = self.ev_scope_body(body);

        for i in 0..scope.allocations {
            self.procedures
                .push(PushImmediate(0, format!("Stack allocation {}", i)));
        }

        for instruction in scope.code {
            self.procedures.push(instruction)
        }

        self.procedures.push(PopStack(scope.allocations));
    }

    fn ev_procedure(
        &mut self,
        symbol: &str,
        args: &[String],
        return_value: &Option<String>,
        body: &[AstNode],
    ) -> StackUsage {
        let proc_address = self.procedures.len();

        self.push_scope(ScopeType::StackFrame);
        for arg in args.iter() {
            let offset = self.get_stack_allocation_offset();
            self.scope.allocations += 1;
            self.scope.variables.insert(
                arg.to_string(),
                Address {
                    addr: offset,
                    kind: AddressKind::StackValue,
                },
            );
        }
        self.ev_procedure_scope(body);
        self.ev_return(&None);
        self.pop_scope(ScopeType::StackFrame);

        // Register function
        self.scope.variables.insert(
            symbol.to_string(),
            Address {
                addr: proc_address,
                kind: AddressKind::Function {
                    argument_count: args.len(),
                    return_value: return_value.is_some(),
                },
            },
        );

        StackUsage::new(0, 0)
    }

    fn ev_string(&mut self, string: &str) -> StackUsage {
        let address = self.heap.len();
        let bytes = string.as_bytes();

        for byte in bytes {
            self.heap.push(*byte as i64);
        }
        self.push_instruction(Instruction::PushImmediate(
            address as i64,
            format!("String pointer"),
        ));
        StackUsage::new(0, 1)
    }

    fn ev_variable_value(&mut self, symbol: &str) -> StackUsage {
        use self::AddressKind::*;
        let address = self.find_var_address(symbol).expect(&format!(
            "Variable symbol '{}' not found in the current scope",
            symbol
        ));
        match address.kind {
            StackValue | ConstStackValue => {
                let offset = address.addr;
                self.push_instruction(Instruction::SLoad(offset as i64));
                StackUsage::new(0, 1)
            }
            _ => panic!("{:?}", address),
        }
    }

    fn ev_assignment(&mut self, symbol: &str, expr: &AstNode, is_declaration: bool) -> StackUsage {
        use crate::ast::AstNode::*;

        let address = self.find_var_address(symbol).expect(&format!(
            "Variable symbol '{}' not found in the current scope",
            symbol
        ));
        if address.kind == AddressKind::ConstStackValue && !is_declaration {
            panic!("Can not assign to constant value");
        }

        let stack_usage = match expr {
            Primitive(val) => self.ev_intermediate(*val),
            _ => self.ev_node(expr),
        };
        assert_eq!(stack_usage.pushed, 1);

        self.push_instruction(Instruction::SStore(address.addr as i64));
        StackUsage::new(stack_usage.popped, 0)
    }

    fn ev_declaration(&mut self, symbol: &str, expr: &AstNode, is_constant: bool) -> StackUsage {
        let offset = self.get_stack_allocation_offset();
        self.scope.allocations += 1;
        if is_constant {
            self.scope.variables.insert(
                symbol.to_string(),
                Address {
                    addr: offset,
                    kind: AddressKind::ConstStackValue,
                },
            );
        } else {
            self.scope.variables.insert(
                symbol.to_string(),
                Address {
                    addr: offset,
                    kind: AddressKind::StackValue,
                },
            );
        }

        self.ev_assignment(symbol, expr, true)
    }

    fn ev_call(&mut self, symbol: &str, args: &[AstNode]) -> StackUsage {
        let address = self.find_fn_address(symbol).expect(&format!(
            "Function symbol '{}' not found in the current scope",
            symbol
        ));

        match address.kind {
            AddressKind::ForeignFunction => {
                let mut popped = 0;
                for arg in args {
                    let usage = self.ev_expression(arg);
                    assert_eq!(1, usage.pushed);
                    popped += usage.popped;
                }
                self.push_instruction(Instruction::PushImmediate(
                    args.len() as i64,
                    format!("Foreign function argument count"),
                ));
                self.push_instruction(Instruction::CallForeign(address.addr, symbol.to_string()));
                let pushed = self.foreign_functions[address.addr].returns;
                StackUsage::new(popped, pushed)
            }
            AddressKind::Function {
                argument_count,
                return_value,
            } => {
                assert_eq!(
                    args.len(),
                    argument_count,
                    "Wong number of arguments passed to function"
                );

                // Return value locations
                self.push_instruction(Instruction::PushImmediate(
                    0,
                    format!("Function return value"),
                ));

                // The number of instructions required to make a function call
                self.push_instruction(Instruction::PushPc(args.len() + CALL_INSTRUCTION_COUNT));
                self.push_instruction(Instruction::PushStackFrame);

                let mut popped = 0;
                for arg in args {
                    let usage = self.ev_expression(arg);
                    assert_eq!(1, usage.pushed);
                    popped += usage.popped;
                }
                self.push_instruction(Instruction::SetStackFrame(-(args.len() as i64)));
                self.push_instruction(Instruction::Call(address.addr, symbol.to_string()));
                if return_value {
                    StackUsage::new(popped, 1)
                } else {
                    StackUsage::new(popped, 0)
                }
            }
            _ => unimplemented!(),
        }
    }

    fn ev_scope_body(&mut self, children: &[AstNode]) -> Scope {
        self.push_scope(ScopeType::Scope);

        let mut stack_usage = StackUsage::new(0, 0);
        for node in children {
            stack_usage += self.ev_node(node);
        }
        assert_eq!(stack_usage.popped, stack_usage.pushed);

        self.pop_scope(ScopeType::Scope)
    }

    fn ev_scope(&mut self, children: &[AstNode]) -> StackUsage {
        use self::Instruction::*;
        let child_scope = self.ev_scope_body(children);

        for i in 0..child_scope.allocations {
            self.scope
                .code
                .push(PushImmediate(0, format!("Scope allocation {}", i)));
        }

        for instruction in child_scope.code {
            self.scope.code.push(instruction)
        }

        self.scope.code.push(PopStack(child_scope.allocations));

        StackUsage::new(0, 0)
    }

    fn ev_prefix_operation(&mut self, op: ArithmeticOp, expr1: &AstNode) -> StackUsage {
        self.push_instruction(Instruction::PushImmediate(
            0,
            format!("Prefix operation return value"),
        ));
        let diff = self.ev_expression(expr1);
        assert_eq!(1, diff.pushed);
        match op {
            ArithmeticOp::Add => self.push_instruction(Instruction::AddI),
            ArithmeticOp::Sub => self.push_instruction(Instruction::SubI),
            _ => panic!("Invalid prefix operation: {:?}", op),
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
            ArithmeticOp::Div => self.push_instruction(Instruction::DivI),
            ArithmeticOp::Eq => self.push_instruction(Instruction::EqI),
        };
        StackUsage::new(diff1.popped + diff2.popped, 1)
    }

    fn ev_expression(&mut self, expr: &AstNode) -> StackUsage {
        use crate::ast::AstNode::*;
        match expr {
            Op(op, expr1, expr2) => self.ev_operation(*op, &expr1, &expr2),
            Primitive(primitive) => self.ev_intermediate(*primitive),
            GetVariable(symbol) => self.ev_variable_value(symbol),
            Call(symbol, args) => self.ev_call(symbol, args),
            _ => panic!("Unsupported node here {:?}", expr),
        }
    }

    fn ev_intermediate(&mut self, value: i64) -> StackUsage {
        self.push_instruction(Instruction::PushImmediate(value, format!("Constant value")));
        StackUsage::new(0, 1)
    }
}
