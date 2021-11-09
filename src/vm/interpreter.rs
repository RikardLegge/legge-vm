use crate::vm::ast::{DebugSymbols, NodeID};
use crate::vm::bytecode;
use crate::vm::bytecode::{Bytecode, OPCode, SFOffset};
use crate::vm::runtime::Runtime;
use crate::{vm, LogLevel};
use std::cell::RefCell;
use std::cmp::Ordering;
use std::fmt::Formatter;
use std::ops::{Add, Div, Mul, Sub};
use std::rc::Rc;
use std::time::Duration;
use std::{fmt, mem};

use crate::Err;

pub type Result<V = ()> = crate::Result<V>;

pub struct Interpreter<'a> {
    pub runtime: &'a Runtime,
    pub interrupt: &'a dyn Fn(bytecode::Value),
    pub log_level: LogLevel,
}

pub struct InterpreterState<'a> {
    pc: Option<usize>,
    pending_frame: Option<StackFrame>,
    frame: StackFrame,
    stack: Vec<Value>,
    symbols: Option<&'a DebugSymbols>,
    stack_max: usize,
    executed_instructions: usize,

    interpreter: &'a Interpreter<'a>,
}
impl<'a> Interpreter<'a> {
    pub fn new(
        runtime: &'a Runtime,
        log_level: LogLevel,
        interrupt: &'a dyn Fn(bytecode::Value),
    ) -> Self {
        Interpreter {
            runtime,
            log_level,
            interrupt,
        }
    }

    pub fn run(&mut self, code: &'a Bytecode) {
        let start = vm::start_timer();

        let stack_size = 10000000;
        let mut state = InterpreterState::new(&self, stack_size);
        state.run(&code);

        let duration = vm::stop_timer(start);
        if self.log_level >= LogLevel::LogTiming {
            let inst_duration = if state.executed_instructions == 0 {
                Duration::new(0, 0)
            } else {
                duration / state.executed_instructions as u32
            };
            println!("Avg Instruction: {:?}", inst_duration)
        }
    }
}

impl<'a> InterpreterState<'a> {
    pub fn new(interpreter: &'a Interpreter<'a>, stack_size: usize) -> Self {
        InterpreterState {
            interpreter,
            stack_max: stack_size,

            pending_frame: None,
            frame: StackFrame {
                stack_pointer: 0,
                closure: Some(Rc::new(RefCell::new(Closure {
                    parent: None,
                    stack: Vec::new(),
                }))),
            },
            pc: Some(0),
            symbols: None,
            stack: Vec::with_capacity(stack_size),
            executed_instructions: 0,
        }
    }

    pub fn exit(&mut self) {
        self.pop_stack_count(self.stack.len()).unwrap();
        self.frame = StackFrame {
            stack_pointer: 0,
            closure: None,
        };
        self.pc = None;
    }

    pub fn execute(&mut self, cmd: &OPCode, node_id: NodeID) -> Result {
        self.executed_instructions += 1;
        if self.interpreter.log_level >= LogLevel::LogEval {
            let line = self.symbols.and_then(|s| s.get_line(node_id)).unwrap_or(0);
            self.debug_log(
                LogLevel::LogEval,
                &format!(
                    "Line: {:>4} PC: {:>4} Inst: {:<40} SP: {:>4} Stack: {:?}",
                    line,
                    format!(
                        "{}",
                        match self.pc {
                            Some(pc) => pc.to_string(),
                            None => "None".into(),
                        }
                    ),
                    format!("{:?}", cmd),
                    self.frame.stack_pointer,
                    self.stack
                ),
            );
        }
        let result = self.run_command(cmd);
        result
    }

    fn run(&mut self, code: &'a Bytecode) {
        if code.code.len() > 0 {
            self.symbols = code.debug_symbols.as_ref();
            loop {
                if let Some(pc) = self.pc {
                    if let Some(cmd) = code.code.get(pc) {
                        self.pc = Some(pc + 1);
                        if let Err(err) = self.execute(&cmd.op, cmd.node_id) {
                            panic!("{:?}", err.details);
                        }
                    } else {
                        panic!("Invalid PC");
                    }
                } else {
                    break;
                }
            }
            self.symbols = None;
        }

        if self.stack.len() > 0 {
            dbg!(&self.stack);
            panic!("Interpreter stopped without an empty stack")
        }
    }
}

impl<'a> InterpreterState<'a> {
    fn debug_log(&self, level: LogLevel, info: &str) {
        if self.interpreter.log_level >= level {
            println!("{}", info);
        }
    }

    fn pop_stack_count(&mut self, count: usize) -> Result {
        for _ in 0..count {
            let ok = self.stack.pop();
            if ok.is_none() {
                return Err(Err::new("Stack empty, can not pop"));
            }
        }
        Ok(())
    }

    fn pop_stack(&mut self) -> Result<Value> {
        match self.stack.pop() {
            Some(val) => Ok(val),
            None => Err(Err::new("Stack empty, can not pop")),
        }
    }

    fn push_stack(&mut self, value: Value) -> Result {
        if self.stack.len() < self.stack_max {
            self.stack.push(value);
            Ok(())
        } else {
            Err(Err::new("Stack overflow"))
        }
    }

    fn foreign_function_arguments(&mut self) -> Result<Vec<bytecode::Value>> {
        let count = self.stack.len() - self.frame.stack_pointer;
        let mut arguments = Vec::with_capacity(count);
        for _ in 0..count {
            let argument = self.pop_stack()?;
            arguments.push(argument.into_bytecode().unwrap());
        }
        arguments.reverse();
        Ok(arguments)
    }

    fn value<'b>(mut value: &'b mut Value, struct_index: &Option<Vec<usize>>) -> &'b mut Value {
        if let Some(struct_index) = struct_index {
            for index in struct_index {
                if let Value::Struct(fields) = value {
                    value = &mut fields[*index];
                } else {
                    unreachable!()
                }
            }
        }
        value
    }

    fn run_command(&mut self, cmd: &OPCode) -> Result {
        use self::OPCode::*;
        match cmd {
            AddI => {
                let n1 = self.pop_stack()?;
                let n2 = self.pop_stack()?;
                let sum = n2 + n1;
                self.push_stack(sum)?;
            }
            SubI => {
                let n1 = self.pop_stack()?;
                let n2 = self.pop_stack()?;
                let diff = n2 - n1;
                self.push_stack(diff)?;
            }
            MulI => {
                let n1 = self.pop_stack()?;
                let n2 = self.pop_stack()?;
                let prod = n2 * n1;
                self.push_stack(prod)?;
            }
            DivI => {
                let n1 = self.pop_stack()?;
                let n2 = self.pop_stack()?;
                let kvote = n2 / n1;
                self.push_stack(kvote)?;
            }
            Eq => {
                let n1 = self.pop_stack()?;
                let n2 = self.pop_stack()?;
                let eq = n1 == n2;
                self.push_stack(Value::Bool(eq))?;
            }
            GEq => {
                let n2 = self.pop_stack()?;
                let n1 = self.pop_stack()?;
                let eq = n1 >= n2;
                self.push_stack(Value::Bool(eq))?;
            }
            LEq => {
                let n2 = self.pop_stack()?;
                let n1 = self.pop_stack()?;
                let eq = n1 <= n2;
                self.push_stack(Value::Bool(eq))?;
            }
            PushToClosure(primitive) => {
                let value = primitive.clone();
                self.frame
                    .closure
                    .as_ref()
                    .unwrap()
                    .borrow_mut()
                    .stack
                    .push(Value::from(value, &self.frame.closure));
            }
            SStore(offset) => match offset {
                SFOffset::Closure {
                    offset,
                    depth,
                    field,
                } => {
                    let index = *offset;
                    let mut depth = *depth;
                    let mut closure = Rc::clone(self.frame.closure.as_ref().unwrap());
                    while depth > 0 {
                        let parent_closure;
                        match &closure.as_ref().borrow().parent {
                            Some(parent_pointer) => parent_closure = Rc::clone(parent_pointer),
                            None => panic!("Invalid closure reference when storing value"),
                        }
                        closure = parent_closure;
                        depth -= 1;
                    }
                    let value = self.pop_stack()?;
                    let target_root = &mut closure.borrow_mut().stack[index];
                    let target = Self::value(target_root, field);
                    *target = value;
                }
                SFOffset::Stack { offset, field } => {
                    let index = (self.frame.stack_pointer as isize + *offset) as usize;
                    let value = self.pop_stack()?;
                    let target_root = &mut self.stack[index];
                    let target = Self::value(target_root, field);
                    *target = value;
                }
            },
            SLoad(offset) => match offset {
                SFOffset::Closure {
                    offset: index,
                    depth,
                    field,
                } => {
                    let mut depth = *depth;
                    let mut closure = Rc::clone(self.frame.closure.as_ref().unwrap());
                    while depth > 0 {
                        let parent_closure;
                        match &closure.as_ref().borrow().parent {
                            Some(parent_pointer) => parent_closure = Rc::clone(parent_pointer),
                            None => panic!("Invalid closure reference when storing value"),
                        }
                        closure = parent_closure;
                        depth -= 1;
                    }
                    let value_root = &mut closure.as_ref().borrow_mut().stack[*index];
                    let value = Self::value(value_root, field);
                    self.push_stack(value.clone())?;
                }
                SFOffset::Stack { offset, field } => {
                    let index = self.frame.stack_pointer as isize + *offset;
                    let value_root = &mut self.stack[index as usize];
                    let value = Self::value(value_root, field);
                    let value = value.clone();
                    self.push_stack(value)?;
                }
            },
            PushImmediate(primitive) => {
                self.push_stack(Value::from(primitive.clone(), &self.frame.closure))?;
            }
            PopStack(count) => {
                self.pop_stack_count(*count)?;
            }
            PushPc(offset) => {
                let pc = self.pc.unwrap();
                let new_pc = (pc as isize + *offset) as usize;
                self.push_stack(Value::PC(new_pc))?;
            }
            Branch(offset) => {
                let pc = self.pc.unwrap();
                let new_pc = (pc as isize + *offset) as usize;
                self.pc = Some(new_pc);
            }
            BranchIfNot(offset) => match self.pop_stack()? {
                Value::Bool(cond) => {
                    if !cond {
                        let pc = self.pc.unwrap();
                        let new_pc = (pc as isize + *offset) as usize;
                        self.pc = Some(new_pc);
                    }
                }
                _ => unreachable!(),
            },
            PopPc => match self.pop_stack()? {
                Value::PC(addr) => {
                    self.pc = Some(addr);
                }
                _ => unreachable!(),
            },
            Jump => {
                let val = self.pop_stack()?;

                assert!(self.pending_frame.is_some());
                let frame = mem::replace(&mut self.pending_frame, None);
                self.frame = frame.unwrap();

                assert!(self.frame.closure.is_none());
                let closure = Closure {
                    parent: None,
                    stack: Vec::new(),
                };
                self.frame.closure = Some(Rc::new(RefCell::new(closure)));

                match val {
                    Value::ProcAddress(addr, parent_stack_frame) => {
                        self.frame.closure.as_ref().unwrap().borrow_mut().parent =
                            parent_stack_frame;
                        self.pc = Some(addr);
                    }
                    Value::RuntimeFn(id) => {
                        let args = self.foreign_function_arguments()?;
                        let func = self.interpreter.runtime.functions[id];
                        let returns = func(self, args)?;
                        // Just make sure that the function has not set the pc to None
                        // If pc is none then we will terminate
                        if self.pc != None {
                            self.run_command(&PopStackFrame)?;
                            self.run_command(&PopPc)?;
                            if let Some(returns) = returns {
                                let value = Value::from(returns, &self.frame.closure);
                                if let Some(&Value::Unset) = self.stack.last() {
                                    let return_index = self.stack.len() - 1;
                                    self.stack[return_index] = value;
                                } else {
                                    panic!("Invalid stack value");
                                }
                            }
                        }
                    }
                    _ => unimplemented!("{:?}", val),
                }
            }
            PrepareStackFrame(arg_count) => {
                let frame = StackFrame {
                    stack_pointer: self.stack.len() - arg_count,
                    closure: None,
                };
                self.pending_frame = Some(frame);
            }
            PushStackFrame => {
                let frame = self.frame.clone();
                self.push_stack(Value::StackFrame(frame))?;
            }
            PopStackFrame => match self.pop_stack()? {
                Value::StackFrame(frame) => {
                    self.frame = frame;
                }
                _ => unreachable!(),
            },
            Halt => {
                self.exit();
            }
            Yield => {
                let value = self.pop_stack()?;
                (self.interpreter.interrupt)(value.into_bytecode().unwrap());
            }
            _ => panic!("Instruction not implemented: {:?}", cmd),
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
struct Closure {
    pub parent: Option<Rc<RefCell<Closure>>>,
    pub stack: Vec<Value>,
}

#[derive(Debug, Clone)]
struct StackFrame {
    stack_pointer: usize,
    closure: Option<Rc<RefCell<Closure>>>,
}

#[derive(Clone)]
enum Value {
    Unset,
    Int(isize),
    Float(f64),
    Bool(bool),
    String(String),
    ProcAddress(usize, Option<Rc<RefCell<Closure>>>),

    PC(usize),
    StackFrame(StackFrame),
    RuntimeFn(usize),
    Struct(Vec<Value>),
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Value::ProcAddress(id, _) => {
                write!(f, "Fn_{}", id)
            }
            Value::StackFrame(_) => {
                write!(f, "Frame {{ ... }}")
            }
            Value::Unset => write!(f, "_"),
            Value::Int(val) => write!(f, "{}", val),
            Value::Float(val) => write!(f, "{}", val),
            Value::Bool(val) => write!(f, "{}", val),
            Value::String(val) => write!(f, "{}", val),
            Value::PC(val) => write!(f, "{}", val),
            Value::RuntimeFn(val) => write!(f, "{}", val),
            Value::Struct(val) => write!(f, "{:?}", val),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        use Value::*;
        match (self, other) {
            (Int(a), Int(b)) => a == b,
            (Bool(a), Bool(b)) => a == b,
            (String(a), String(b)) => a == b,
            (ProcAddress(a, _), ProcAddress(b, _)) => a == b,
            _ => unimplemented!(),
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        use Value::*;
        match (self, other) {
            (Int(a), Int(b)) => a.partial_cmp(b),
            (Float(a), Float(b)) => a.partial_cmp(&b),
            (Bool(a), Bool(b)) => a.partial_cmp(b),
            (String(a), String(b)) => a.partial_cmp(b),
            (ProcAddress(a, _), ProcAddress(b, _)) => a.partial_cmp(b),
            _ => unimplemented!(),
        }
    }
}

impl Add for Value {
    type Output = Self;

    fn add(self, other: Self) -> Self::Output {
        use Value::*;
        match (self, other) {
            (Int(a), Int(b)) => Value::Int(a + b),
            (Float(a), Float(b)) => Value::Float(a + b),
            (String(a), String(b)) => Value::String(a + &b),
            _ => unimplemented!(),
        }
    }
}

impl Sub for Value {
    type Output = Self;

    fn sub(self, other: Self) -> Self::Output {
        use Value::*;
        match (self, other) {
            (Int(a), Int(b)) => Value::Int(a - b),
            (Float(a), Float(b)) => Value::Float(a - b),
            _ => unimplemented!(),
        }
    }
}

impl Div for Value {
    type Output = Self;

    fn div(self, other: Self) -> Self::Output {
        use Value::*;
        match (self, other) {
            (Int(a), Int(b)) => Value::Int(a / b),
            (Float(a), Float(b)) => Value::Float(a / b),
            _ => unimplemented!(),
        }
    }
}

impl Mul for Value {
    type Output = Self;

    fn mul(self, other: Self) -> Self::Output {
        use Value::*;
        match (self, other) {
            (Int(a), Int(b)) => Value::Int(a * b),
            (Float(a), Float(b)) => Value::Float(a * b),
            _ => unimplemented!(),
        }
    }
}

impl Value {
    fn into_bytecode(self) -> Option<bytecode::Value> {
        match self {
            Value::Unset => Some(bytecode::Value::Unset),
            Value::Int(val) => Some(bytecode::Value::Int(val)),
            Value::Float(val) => Some(bytecode::Value::Float(val)),
            Value::Bool(val) => Some(bytecode::Value::Bool(val)),
            Value::String(val) => Some(bytecode::Value::String(val)),
            Value::ProcAddress(addr, _) => Some(bytecode::Value::ProcAddress(addr)),
            Value::RuntimeFn(addr) => Some(bytecode::Value::RuntimeFn(addr)),
            Value::Struct(vals) => {
                let mut bc_vals = Vec::with_capacity(vals.len());
                for val in vals {
                    match val.into_bytecode() {
                        Some(bc_val) => bc_vals.push(bc_val),
                        None => return None,
                    }
                }
                Some(bytecode::Value::Struct(bc_vals))
            }
            Value::PC(_) => None,
            Value::StackFrame(_) => None,
        }
    }

    fn from(value: bytecode::Value, closure: &Option<Rc<RefCell<Closure>>>) -> Self {
        match value {
            bytecode::Value::ProcAddress(addr) => Value::ProcAddress(addr, closure.clone()),
            bytecode::Value::Unset => Value::Unset,
            bytecode::Value::Int(val) => Value::Int(val),
            bytecode::Value::Float(val) => Value::Float(val),
            bytecode::Value::Bool(val) => Value::Bool(val),
            bytecode::Value::String(val) => Value::String(val),
            bytecode::Value::RuntimeFn(val) => Value::RuntimeFn(val),
            bytecode::Value::Struct(bc_vals) => {
                let val = bc_vals
                    .into_iter()
                    .map(|val| Value::from(val, closure))
                    .collect();
                Value::Struct(val)
            }
        }
    }
}
