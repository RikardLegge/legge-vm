use crate::ast::{AssignmentType, Ast, AstNode, AstNodeBody, NodeID};
use crate::bytecode::Instruction::Branch;
use crate::foreign_functions::ForeignFunction;
use crate::token::ArithmeticOp;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::mem;
use std::ops::AddAssign;

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
        let global_scope = mem::replace(&mut bc.scope, Scope::new(0));
        assert_eq!(bc.code.len(), 0);
        bc.code = global_scope.code;
        // bc.optimize();
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

    BranchUnlinked(LinkTarget),
    Branch(isize),
    BranchIfUnlinked(LinkTarget),
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

    Panic(String),
}

#[derive(Copy, Clone, Debug, PartialEq, Serialize, Deserialize)]
enum LoopTarget {
    Start,
    End,
}

#[derive(Copy, Clone, Debug, PartialEq, Serialize, Deserialize)]
enum IfTarget {
    Else,
    End,
}

#[derive(Copy, Clone, Debug, PartialEq, Serialize, Deserialize)]
enum LinkTarget {
    If(NodeID, IfTarget),
    Loop(NodeID, LoopTarget),
    Return(NodeID),
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
    Block,
    Loop,
    StackFrame,
}

struct Scope {
    node_id: NodeID,
    parent: Option<Box<Scope>>,
    variables: HashMap<String, Address>,
    allocations: usize,
    tp: ScopeType,
    code: Vec<Instruction>,
}

impl Scope {
    fn new(node_id: NodeID) -> Self {
        Scope {
            node_id,
            parent: None,
            variables: HashMap::new(),
            allocations: 0,
            code: Vec::new(),
            tp: ScopeType::Block,
        }
    }

    fn get_parent_allocations(&self, node_id: NodeID, tp: ScopeType) -> usize {
        if self.node_id == node_id && self.tp == tp {
            return self.allocations;
        }
        if let Some(p) = &self.parent {
            return self.allocations + p.get_parent_allocations(node_id, tp);
        }
        panic!("Parent not found");
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

        Bytecode {
            code,
            data,
            procedure_address,
        }
    }

    fn new(foreign_functions: &'a [ForeignFunction]) -> Self {
        let scope = Scope::new(0);
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

    fn next_stack_allocation_offset(&mut self) -> usize {
        let mut offset = self.scope.allocations;
        if !(self.scope.tp == ScopeType::StackFrame) {
            let mut parent = &self.scope.parent;
            while let Some(scope) = parent {
                offset += scope.allocations;
                if scope.tp == ScopeType::StackFrame {
                    parent = &None
                } else {
                    parent = &scope.parent
                }
            }
        }
        self.scope.allocations += 1;
        offset
    }

    fn push_scope(&mut self, node_id: NodeID, tp: ScopeType) {
        let parent = mem::replace(&mut self.scope, Scope::new(node_id));
        self.scope.parent = Some(Box::new(parent));
        self.scope.tp = tp;
    }

    fn pop_scope(&mut self, id: NodeID) -> Scope {
        assert_eq!(self.scope.node_id, id);
        self.pop_scope_unsafe()
    }

    fn pop_scope_unsafe(&mut self) -> Scope {
        let parent = *mem::replace(&mut self.scope.parent, None).unwrap();
        mem::replace(&mut self.scope, parent)
    }

    fn ev_node(&mut self, node: &AstNode) -> StackUsage {
        use crate::ast::AstNodeBody::*;
        match &node.body {
            Op(op, expr1, expr2) => self.ev_operation(*op, &expr1, &expr2),
            PrefixOp(op, expr1) => self.ev_prefix_operation(*op, &expr1),
            Block(children) => self.ev_scope(node.id, children),
            Call(name, args) => self.ev_call(&name, args),
            GetVariable(name) => self.ev_variable_value(&name),
            Assignment(tp, name, expr) => match tp {
                AssignmentType::Assignment => self.ev_assignment(&name, expr, false),
                AssignmentType::Declaration => self.ev_declaration(&name, expr, false),
                AssignmentType::ConstDeclaration => self.ev_declaration(&name, expr, true),
            },
            ProcedureDeclaration(name, args, return_values, body) => {
                self.ev_procedure(node.id, name, &args, &return_values, body)
            }
            Return(proc_id, return_value) => self.ev_return(*proc_id, return_value),
            If(condition, body) => self.ev_if(condition, body),
            Loop(body) => self.ev_loop(node.id, body),
            Break(id) => self.ev_break(*id),
            Comment(_) => StackUsage::new(0, 0),
            _ => panic!("Unsupported node here {:?}", node),
        }
    }

    fn ev_break(&mut self, loop_id: NodeID) -> StackUsage {
        let parent_alloc = self.scope.get_parent_allocations(loop_id, ScopeType::Loop);
        self.scope.code.push(Instruction::PopStack(parent_alloc));
        self.scope
            .code
            .push(Instruction::BranchUnlinked(LinkTarget::Loop(
                loop_id,
                LoopTarget::End,
            )));

        StackUsage {
            popped: 0,
            pushed: 0,
        }
    }

    fn ev_loop(&mut self, node_id: NodeID, body_node: &Box<AstNode>) -> StackUsage {
        self.push_scope(node_id, ScopeType::Loop);
        self.scope.tp = ScopeType::Loop;

        let start = self.scope.code.len();
        if let AstNodeBody::Block(children) = &body_node.body {
            let usage = self.ev_scope(body_node.id, children);
            assert_eq!(usage.pushed, usage.popped);
        } else {
            panic!("The body of a loop statement must be a scope");
        }
        let end = self.scope.code.len();
        let start_of_loop = start as isize - end as isize - 1;
        self.scope.code.push(Instruction::Branch(start_of_loop));

        let mut scope = self.pop_scope(node_id);

        for i in 0..scope.code.len() {
            let inst = &scope.code[i];
            if let &Instruction::BranchUnlinked(t) = inst {
                if let LinkTarget::Loop(id, target) = t {
                    if id == node_id {
                        match target {
                            LoopTarget::Start => scope.code[i] = Branch(-(i as isize) - 1),
                            LoopTarget::End => {
                                scope.code[i] = Branch((scope.code.len() - i) as isize - 1)
                            }
                        }
                    }
                }
            }
        }

        self.scope.code.append(&mut scope.code);

        StackUsage {
            popped: 0,
            pushed: 0,
        }
    }

    fn ev_if(&mut self, condition: &Box<AstNode>, node: &Box<AstNode>) -> StackUsage {
        let usage = self.ev_expression(condition);
        assert_eq!(usage.pushed - usage.popped, 1);
        self.scope.code.push(Instruction::PushImmediate(
            0,
            "Invert increment condition".into(),
        ));
        self.scope.code.push(Instruction::EqI);
        let jump_index = self.scope.code.len();
        self.scope
            .code
            .push(Instruction::BranchIfUnlinked(LinkTarget::If(
                0,
                IfTarget::Else,
            )));

        let start = self.scope.code.len();
        if let AstNodeBody::Block(children) = &node.body {
            let usage = self.ev_scope(node.id, children);
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

    fn ev_return(&mut self, proc_id: NodeID, return_value: &Option<Box<AstNode>>) -> StackUsage {
        let parent_alloc = self
            .scope
            .get_parent_allocations(proc_id, ScopeType::StackFrame);

        self.scope.code.push(Instruction::PopStack(parent_alloc));
        self.scope.code.push(Instruction::PopStackFrame);
        self.scope.code.push(Instruction::PopPc);

        StackUsage::new(0, 0)
    }

    fn ev_procedure_scope(&mut self, node_id: NodeID, body: &[AstNode]) -> Scope {
        use self::Instruction::*;
        let mut scope = self.ev_scope_body(node_id, body);

        for i in 0..scope.allocations {
            self.scope
                .code
                .push(PushImmediate(0, format!("Stack allocation {}", i)));
        }

        self.scope.code.append(&mut scope.code);
        scope
    }

    fn ev_procedure(
        &mut self,
        node_id: NodeID,
        symbol: &str,
        args: &[String],
        return_value: &Option<String>,
        body: &[AstNode],
    ) -> StackUsage {
        let proc_offset = self.procedures.len();
        let proc_address = Address {
            addr: proc_offset,
            kind: AddressKind::Function {
                argument_count: args.len(),
                return_value: return_value.is_some(),
            },
        };

        // Register function globally
        self.scope
            .variables
            .insert(symbol.to_string(), proc_address);
        self.push_scope(node_id, ScopeType::StackFrame);
        // Register function locally
        self.scope
            .variables
            .insert(symbol.to_string(), proc_address);

        for arg in args.iter() {
            let offset = self.next_stack_allocation_offset();
            self.scope.variables.insert(
                arg.to_string(),
                Address {
                    addr: offset,
                    kind: AddressKind::StackValue,
                },
            );
        }
        self.ev_procedure_scope(node_id, body);
        if self.scope.code.len() == 0 || Some(&Instruction::PopPc) != self.scope.code.last() {
            self.ev_return(node_id, &None);
        }
        let mut scope = self.pop_scope(node_id);

        self.procedures.append(&mut scope.code);

        StackUsage::new(0, 0)
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
        use crate::ast::AstNodeBody::*;

        let address = self.find_var_address(symbol).expect(&format!(
            "Variable symbol '{}' not found in the current scope",
            symbol
        ));
        if address.kind == AddressKind::ConstStackValue && !is_declaration {
            panic!("Can not assign to constant value");
        }

        let stack_usage = match expr.body {
            Primitive(val) => self.ev_intermediate(val),
            _ => self.ev_node(expr),
        };
        assert_eq!(stack_usage.pushed, 1);

        self.push_instruction(Instruction::SStore(address.addr as i64));
        StackUsage::new(stack_usage.popped, 0)
    }

    fn ev_declaration(&mut self, symbol: &str, expr: &AstNode, is_constant: bool) -> StackUsage {
        let offset = self.next_stack_allocation_offset();
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

                let pc_index = self.scope.code.len();
                self.push_instruction(Instruction::Panic(
                    "PushPC instruction offset not written correctly".to_string(),
                ));
                self.push_instruction(Instruction::PushStackFrame);

                let mut popped = 0;
                for arg in args {
                    let usage = self.ev_expression(arg);
                    assert_eq!(1, usage.pushed);
                    popped += usage.popped;
                }
                self.push_instruction(Instruction::SetStackFrame(-(args.len() as i64)));
                self.push_instruction(Instruction::Call(address.addr, symbol.to_string()));
                let offset = self.scope.code.len() - pc_index - 1;
                self.scope.code[pc_index] = Instruction::PushPc(offset);
                StackUsage::new(popped, 0)
            }
            _ => unimplemented!(),
        }
    }

    fn ev_scope_body(&mut self, node_id: NodeID, children: &[AstNode]) -> Scope {
        self.push_scope(node_id, ScopeType::Block);

        let mut stack_usage = StackUsage::new(0, 0);
        for node in children {
            stack_usage += self.ev_node(node);
        }
        assert_eq!(stack_usage.popped, stack_usage.pushed);

        self.pop_scope(node_id)
    }

    fn ev_scope(&mut self, node_id: NodeID, children: &[AstNode]) -> StackUsage {
        use self::Instruction::*;
        let mut child_scope = self.ev_scope_body(node_id, children);

        for i in 0..child_scope.allocations {
            self.scope
                .code
                .push(PushImmediate(0, format!("Scope allocation {}", i)));
        }

        self.scope.code.append(&mut child_scope.code);
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
        use crate::ast::AstNodeBody::*;
        match &expr.body {
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
