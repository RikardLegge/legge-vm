use crate::bytecode::{Bytecode, SFOffset, OP};
use crate::runtime::Runtime;
use crate::{bytecode, LogLevel};
use std::cell::RefCell;
use std::mem;
use std::rc::Rc;

#[derive(Debug)]
pub struct Err {
    details: String,
}

impl Err {
    pub fn new(details: &str) -> Err {
        Err {
            details: details.to_string(),
        }
    }
}

pub type Result<V = ()> = std::result::Result<V, Err>;

pub struct Interpreter<'a> {
    pc: Option<usize>,
    pending_frame: Option<StackFrame>,
    frame: StackFrame,
    stack: Vec<Value>,
    pub runtime: &'a Runtime,
    pub log_level: LogLevel,
    pub stack_max: usize,
    pub executed_instructions: usize,
    pub interrupt: &'a dyn Fn(bytecode::Value),
}

impl<'a> Interpreter<'a> {
    pub fn new(runtime: &'a Runtime) -> Self {
        let stack_size = 20;
        Interpreter {
            pending_frame: None,
            frame: StackFrame {
                stack_pointer: 0,
                closure: None,
            },
            pc: Some(0),
            stack: Vec::with_capacity(stack_size),
            runtime,
            log_level: LogLevel::LogNone,
            stack_max: stack_size,
            executed_instructions: 0,
            interrupt: &|_| {},
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

    pub fn execute(&mut self, cmd: &OP) -> Result {
        self.executed_instructions += 1;
        let result = self.run_command(cmd);
        if self.log_level >= LogLevel::LogEval {
            self.debug_log(
                LogLevel::LogEval,
                &format!(
                    "PC: {:>4} SP: {:>4} Inst: {:<40} Stack: {:?}",
                    format!("{:?}", self.pc),
                    self.frame.stack_pointer,
                    format!("{:?}", cmd),
                    self.stack
                ),
            );
        }
        result
    }

    pub fn run(mut self, code: &Bytecode) -> usize {
        loop {
            if let Some(pc) = self.pc {
                if let Some(cmd) = code.code.get(pc) {
                    self.pc = Some(pc + 1);
                    if let Err(err) = self.execute(&cmd.op) {
                        panic!("{:?}", err);
                    }
                } else {
                    panic!("Invalid PC");
                }
            } else {
                break;
            }
        }
        if self.stack.len() > 0 {
            dbg!(&self.stack);
            panic!("Interpreter stopped without an empty stack")
        }
        self.executed_instructions
    }
}

impl<'a> Interpreter<'a> {
    fn debug_log(&self, level: LogLevel, info: &str) {
        if self.log_level >= level {
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

    fn pop_stack_int(&mut self) -> Result<isize> {
        match self.pop_stack()? {
            Value::Int(int) => Ok(int),
            val => Err(Err::new(&format!(
                "Stack value is not of type int {:?}",
                val
            ))),
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

    fn get_foreign_function_arguments(&mut self) -> Result<Vec<bytecode::Value>> {
        let count = self.stack.len() - self.frame.stack_pointer;
        let mut arguments = Vec::with_capacity(count);
        for _ in 0..count {
            let argument = self.pop_stack()?;
            arguments.push(argument.into_bytecode().unwrap());
        }
        arguments.reverse();
        Ok(arguments)
    }

    fn run_command(&mut self, cmd: &OP) -> Result {
        use self::OP::*;
        match cmd {
            AddI => {
                let n1 = self.pop_stack_int()?;
                let n2 = self.pop_stack_int()?;
                let sum = n2 + n1;
                self.push_stack(Value::Int(sum))?;
            }
            SubI => {
                let n1 = self.pop_stack_int()?;
                let n2 = self.pop_stack_int()?;
                let diff = n2 - n1;
                self.push_stack(Value::Int(diff))?;
            }
            MulI => {
                let n1 = self.pop_stack_int()?;
                let n2 = self.pop_stack_int()?;
                let prod = n2 * n1;
                self.push_stack(Value::Int(prod))?;
            }
            DivI => {
                let n1 = self.pop_stack_int()?;
                let n2 = self.pop_stack_int()?;
                let kvote = n2 / n1;
                self.push_stack(Value::Int(kvote))?;
            }
            Eq => {
                let n1 = self.pop_stack()?;
                let n2 = self.pop_stack()?;
                let eq = n1 == n2;
                self.push_stack(Value::Bool(eq))?;
            }
            PushToClosure => {
                self.frame
                    .closure
                    .as_ref()
                    .unwrap()
                    .borrow_mut()
                    .stack
                    .push(Value::Unset);
            }
            SStore(offset) => match offset {
                SFOffset::Closure(index, depth) => {
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
                    closure.borrow_mut().stack[*index] = value;
                }
                SFOffset::Stack(offset) => {
                    let index = self.frame.stack_pointer as isize + *offset;
                    assert!(index >= 0);
                    self.stack[index as usize] = self.pop_stack()?;
                }
            },
            SLoad(offset) => match offset {
                SFOffset::Closure(index, depth) => {
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
                    let value = closure.as_ref().borrow().stack[*index].clone();
                    self.push_stack(value)?;
                }
                SFOffset::Stack(offset) => {
                    let index = self.frame.stack_pointer as isize + *offset;
                    let value = self.stack[index as usize].clone();
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
            BranchIf(offset) => match self.pop_stack()? {
                Value::Bool(cond) => {
                    if cond {
                        let pc = self.pc.unwrap();
                        let new_pc = (pc as isize + *offset) as usize;
                        self.pc = Some(new_pc);
                    }
                }
                _ => unreachable!(),
            },
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
                        let mut args = self.get_foreign_function_arguments()?;
                        let func = self.runtime.functions[id].function;
                        let returns = func(self, &mut args)?;
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
                    _ => unimplemented!(),
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
                (self.interrupt)(value.into_bytecode().unwrap());
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

#[derive(Debug, Clone)]
enum Value {
    Unset,
    Int(isize),
    Bool(bool),
    String(String),
    ProcAddress(usize, Option<Rc<RefCell<Closure>>>),

    PC(usize),
    StackFrame(StackFrame),
    RuntimeFn(usize),
    Struct(Vec<Value>),
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

impl Value {
    fn into_bytecode(self) -> Option<bytecode::Value> {
        match self {
            Value::Unset => Some(bytecode::Value::Unset),
            Value::Int(val) => Some(bytecode::Value::Int(val)),
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
            bytecode::Value::Bool(val) => Value::Bool(val),
            bytecode::Value::String(val) => Value::String(val),
            bytecode::Value::RuntimeFn(val) => Value::RuntimeFn(val),
            bytecode::Value::Struct(bc_vals) => {
                let val = bc_vals
                    .iter()
                    .map(|val| Value::from(val.clone(), closure))
                    .collect();
                Value::Struct(val)
            }
        }
    }
}
