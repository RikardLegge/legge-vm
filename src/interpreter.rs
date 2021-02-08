use crate::bytecode;
use crate::bytecode::{Bytecode, SFOffset, OP};
use crate::runtime::Runtime;
use std::mem;

struct Closure {
    pub parent: Option<usize>,
    pub stack: Vec<Value>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct StackFrame {
    stack_pointer: usize,
    closure_id: Option<usize>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Value {
    Unset,
    Int(isize),
    Bool(bool),
    String(String),
    ProcAddress(usize, Option<usize>),

    PC(usize),
    StackFrame(StackFrame),
    RuntimeFn(usize),
}

pub struct Interpreter<'a> {
    pc: Option<usize>,
    pending_frame: Option<StackFrame>,
    frame: StackFrame,
    closures: Vec<Closure>,
    pub stack: Vec<Value>,
    pub runtime: &'a Runtime,
    pub log_level: InterpLogLevel,
    pub stack_max: usize,
    pub executed_instructions: usize,
    pub interrupt: &'a dyn Fn(Value),
}

#[allow(dead_code)]
#[derive(Debug, PartialOrd, PartialEq)]
pub enum InterpLogLevel {
    LogNone = 0,
    LogDebug = 1,
    LogEval = 2,
}

#[derive(Debug)]
pub struct InterpError {
    details: String,
}

impl InterpError {
    pub fn new(details: &str) -> InterpError {
        InterpError {
            details: details.to_string(),
        }
    }
}

pub type InterpResult<V = Value> = Result<V, InterpError>;

impl<'a> Interpreter<'a> {
    pub fn new(runtime: &'a Runtime) -> Self {
        let stack_size = 20;
        Interpreter {
            pending_frame: None,
            frame: StackFrame {
                stack_pointer: 0,
                closure_id: None,
            },
            pc: Some(0),
            stack: Vec::with_capacity(stack_size),
            closures: Vec::new(),
            runtime,
            log_level: InterpLogLevel::LogNone,
            stack_max: stack_size,
            executed_instructions: 0,
            interrupt: &|_| {},
        }
    }

    pub fn set_log_level(&mut self, level: InterpLogLevel) {
        self.log_level = level;
    }

    fn debug_log(&self, level: InterpLogLevel, info: &str) {
        if self.log_level >= level {
            println!("{}", info);
        }
    }

    pub fn pop_stack_count(&mut self, count: usize) -> InterpResult<()> {
        for _ in 0..count {
            let ok = self.stack.pop();
            if ok.is_none() {
                return Err(InterpError::new("Stack empty, can not pop"));
            }
        }
        Ok(())
    }

    pub fn pop_stack(&mut self) -> InterpResult {
        match self.stack.pop() {
            Some(val) => Ok(val),
            None => Err(InterpError::new("Stack empty, can not pop")),
        }
    }

    pub fn pop_stack_int(&mut self) -> InterpResult<isize> {
        match self.pop_stack()? {
            Value::Int(int) => Ok(int),
            val => Err(InterpError::new(&format!(
                "Stack value is not of type int {:?}",
                val
            ))),
        }
    }

    pub fn push_stack(&mut self, value: Value) -> InterpResult<()> {
        if self.stack.len() < self.stack_max {
            self.stack.push(value);
            Ok(())
        } else {
            Err(InterpError::new("Stack overflow"))
        }
    }

    pub fn get_foreign_function_arguments(&mut self) -> Result<Vec<Value>, InterpError> {
        let count = self.stack.len() - self.frame.stack_pointer;
        let mut arguments = Vec::with_capacity(count);
        for _ in 0..count {
            let argument = self.pop_stack()?;
            arguments.push(argument);
        }
        arguments.reverse();
        Ok(arguments)
    }

    pub fn exit(&mut self) {
        self.pc = None;
    }

    pub fn execute(&mut self, cmd: &OP) -> InterpResult<()> {
        self.executed_instructions += 1;
        let result = self.run_command(cmd);
        if self.log_level >= InterpLogLevel::LogEval {
            self.debug_log(
                InterpLogLevel::LogEval,
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

    fn run_command(&mut self, cmd: &OP) -> InterpResult<()> {
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
                self.closures[self.frame.closure_id.unwrap()]
                    .stack
                    .push(Value::Unset);
            }
            SStore(offset) => match offset {
                SFOffset::Closure(index, depth) => {
                    let mut depth = *depth;
                    let mut closure_id = self.frame.closure_id.unwrap();
                    while depth > 0 {
                        match self.closures[closure_id].parent {
                            Some(parent_pointer) => closure_id = parent_pointer,
                            None => panic!("Invalid closure reference when storing value"),
                        }
                        depth -= 1;
                    }
                    let value = self.pop_stack()?;
                    let closure = &mut self.closures[closure_id];
                    closure.stack[*index] = value;
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
                    let mut closure_id = self.frame.closure_id.unwrap();
                    while depth > 0 {
                        match self.closures[closure_id].parent {
                            Some(parent_pointer) => closure_id = parent_pointer,
                            None => panic!("Invalid closure reference when loading value"),
                        }
                        depth -= 1;
                    }
                    let closure = &mut self.closures[closure_id];
                    let value = closure.stack[*index].clone();
                    self.push_stack(value)?;
                }
                SFOffset::Stack(offset) => {
                    let index = self.frame.stack_pointer as isize + *offset;
                    let value = self.stack[index as usize].clone();
                    self.push_stack(value)?;
                }
            },
            PushImmediateBytecode(primitive) => {
                let value = match primitive {
                    bytecode::Value::ProcAddress(addr) => {
                        Value::ProcAddress(*addr, self.frame.closure_id)
                    }
                    bytecode::Value::Unset => Value::Unset,
                    bytecode::Value::Int(val) => Value::Int(*val),
                    bytecode::Value::Bool(val) => Value::Bool(*val),
                    bytecode::Value::String(val) => Value::String(val.clone()),
                    bytecode::Value::RuntimeFn(val) => Value::RuntimeFn(*val),
                };
                self.push_stack(value)?;
            }
            PushImmediate(primitive) => {
                let value = (*primitive).clone();
                self.push_stack(value)?;
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

                let closure_id = self.closures.len();
                let closure = Closure {
                    parent: None,
                    stack: Vec::new(),
                };
                self.closures.push(closure);
                assert!(self.frame.closure_id.is_none());
                self.frame.closure_id = Some(closure_id);

                match val {
                    Value::ProcAddress(addr, parent_stack_frame) => {
                        self.closures[self.frame.closure_id.unwrap()].parent = parent_stack_frame;
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
                                self.push_stack(returns)?;
                            }
                        }
                    }
                    _ => unimplemented!(),
                }
            }
            PrepareStackFrame(arg_count) => {
                let frame = StackFrame {
                    stack_pointer: self.stack.len() - arg_count,
                    closure_id: None,
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
                (self.interrupt)(value);
            }
            _ => panic!("Instruction not implemented: {:?}", cmd),
        }
        Ok(())
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
