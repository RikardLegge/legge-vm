use crate::bytecode::{Bytecode, Value, OP};
use crate::runtime::Runtime;

pub struct Interpreter<'a> {
    pc: usize,
    frame_pointer: usize,
    pub stack: Vec<Value>,
    pub runtime: &'a Runtime,
    pub log_level: InterpLogLevel,
    pub stack_max: usize,
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
        Interpreter {
            frame_pointer: 0,
            pc: 0,
            stack: Vec::with_capacity(1000),
            runtime,
            log_level: InterpLogLevel::LogDebug,
            stack_max: 10000,
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
        if self.stack.len() >= count {
            for _ in 0..count {
                self.stack.pop();
            }
            Ok(())
        } else {
            Err(InterpError::new("Stack empty, can not pop"))
        }
    }

    pub fn pop_stack(&mut self) -> InterpResult {
        match self.stack.pop() {
            Some(val) => Ok(val),
            None => Err(InterpError::new("Stack empty, can not pop")),
        }
    }

    pub fn pop_stack_int(&mut self) -> InterpResult<isize> {
        match self.pop_stack() {
            Ok(val) => match val {
                Value::Int(int) => Ok(int),
                _ => Err(InterpError::new(&format!(
                    "Stack value is not of type int {:?}",
                    val
                ))),
            },
            Err(e) => Err(e),
        }
    }

    pub fn pop_stack_addr(&mut self) -> InterpResult<usize> {
        match self.pop_stack() {
            Ok(val) => match val {
                Value::InstructionAddress(addr) => Ok(addr),
                _ => Err(InterpError::new(&format!(
                    "Stack value is not of type address {:?}",
                    val
                ))),
            },
            Err(e) => Err(e),
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
        let count = self.stack.len() - self.frame_pointer;
        let mut arguments = Vec::with_capacity(count);
        for _ in 0..count {
            let argument = self.pop_stack()?;
            arguments.push(argument);
        }
        arguments.reverse();
        Ok(arguments)
    }

    fn run_command(&mut self, cmd: &OP) -> InterpResult<()> {
        use self::OP::*;

        if self.log_level >= InterpLogLevel::LogDebug {
            self.debug_log(
                InterpLogLevel::LogDebug,
                &format!(
                    "PC: {:>4} SP: {:>4} Inst: {:<40} Stack: {:?}",
                    self.pc,
                    self.frame_pointer,
                    format!("{:?}", cmd),
                    self.stack
                ),
            );
        }
        self.pc += 1;
        match cmd {
            AddI => {
                let n1 = self.pop_stack_int()?;
                let n2 = self.pop_stack_int()?;
                let sum = n2 + n1;
                self.push_stack(Value::Int(sum))?;
                // self.debug_log(LogEval, &format!("{} + {} = {}", n2, n1, sum));
            }
            SubI => {
                let n1 = self.pop_stack_int()?;
                let n2 = self.pop_stack_int()?;
                let diff = n2 - n1;
                self.push_stack(Value::Int(diff))?;
                // self.debug_log(LogEval, &format!("{} - {} = {}", n2, n1, diff));
            }
            MulI => {
                let n1 = self.pop_stack_int()?;
                let n2 = self.pop_stack_int()?;
                let prod = n2 * n1;
                self.push_stack(Value::Int(prod))?;
                // self.debug_log(LogEval, &format!("{} * {} = {}", n2, n1, prod));
            }
            DivI => {
                let n1 = self.pop_stack_int()?;
                let n2 = self.pop_stack_int()?;
                let kvote = n2 / n1;
                self.push_stack(Value::Int(kvote))?;
                // self.debug_log(LogEval, &format!("{} / {} = {}", n2, n1, kvote));
            }
            CmpI => {
                let n1 = self.pop_stack_int()?;
                let n2 = self.pop_stack_int()?;
                let cmp = if n2 == n1 {
                    0
                } else if n2 < n1 {
                    -1
                } else {
                    1
                };
                self.push_stack(Value::Int(cmp))?;
                // self.debug_log(LogEval, &format!("({} == {}) = {}", n2, n1, eq));
            }
            SStore(offset) => {
                let value = self.pop_stack()?;
                let index = self.frame_pointer as isize + *offset;
                assert!(index >= 0);
                self.stack[index as usize] = value;
            }
            SLoad(offset) => {
                let index = self.frame_pointer as isize + *offset;
                let value = self.stack[index as usize].clone();
                self.push_stack(value)?;
                // self.debug_log(LogEval, &format!("{}", value));
            }
            PushImmediate(primitive) => {
                // We clone here since all immediate values are constants and
                // should never change.
                let value = (*primitive).clone();
                self.push_stack(value)?;
            }
            PopStack(count) => {
                self.pop_stack_count(*count)?;
            }
            PushPc(offset) => {
                self.push_stack(Value::InterpreterAddress(
                    (offset + self.pc as isize) as usize,
                ))?;
            }
            Branch(pc) => {
                self.pc = (self.pc as isize + *pc) as usize;
            }
            BranchIf(pc) => {
                let value = self.pop_stack_int()?;
                if value != 0 {
                    self.pc = (self.pc as isize + *pc) as usize;
                }
            }
            PopPc => {
                self.pc = self.pop_stack_addr()?;
            }
            Jump => {
                let val = self.pop_stack()?;
                match val {
                    Value::RuntimePointer(ident) => {
                        let rfd = {
                            let mut function = None;
                            for func in &self.runtime.functions {
                                if func.name == ident {
                                    function = Some(func)
                                }
                            }
                            if let Some(function) = function {
                                function
                            } else {
                                unimplemented!()
                            }
                        };

                        let mut args = self.get_foreign_function_arguments()?;
                        let _ = (rfd.function)(&mut args);
                    }
                    Value::Int(offset) => {
                        self.pc = (self.pc as isize + offset) as usize;
                    }
                    Value::InstructionAddress(addr) => {
                        self.pc = addr;
                    }
                    _ => unimplemented!(),
                }
            }
            PushStackFrame => {
                self.push_stack(Value::InterpreterAddress(self.frame_pointer))?;
            }
            SetStackFrame(offset) => {
                let end = self.stack.len() as isize;
                self.frame_pointer = (end + *offset) as usize;
            }
            PopStackFrame => {
                self.frame_pointer = self.pop_stack_addr()?;
            }
            NoOp => {}
            _ => panic!("Instruction not implemented: {:?}", cmd),
        }
        Ok(())
    }

    fn setup(&mut self) {
        self.pc = 0;
        self.frame_pointer = 0;
        self.stack.clear();
    }

    pub fn run(&mut self, code: &Bytecode) -> usize {
        self.setup();

        let mut instructions = 0;

        while let Some(cmd) = code.code.get(self.pc) {
            instructions += 1;
            if cmd.op == OP::Halt {
                self.debug_log(
                    InterpLogLevel::LogDebug,
                    &format!(
                        "PC: {:>4} SP: {:>4} Inst: {:<40} Stack: {:?}\n",
                        self.pc,
                        self.frame_pointer,
                        format!("{:?}", cmd.op),
                        self.stack
                    ),
                );
                break;
            }
            if let Err(err) = self.run_command(&cmd.op) {
                panic!("{:?}", err);
            }
        }
        if self.stack.len() > 0 {
            dbg!(&self.stack);
            panic!("Interpreter stopped without an empty stack")
        }
        instructions
    }
}
