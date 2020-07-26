use crate::bytecode::{Bytecode, Value, OP};
use crate::foreign_functions::ForeignFunction;

pub struct Interpreter<'a> {
    pc: usize,
    frame_pointer: usize,
    pub stack: Vec<Value>,
    pub foreign_functions: &'a [ForeignFunction],
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

pub type ForeignInterpResult = Result<Vec<i64>, InterpError>;
pub type InterpResult<V = Value> = Result<V, InterpError>;

impl<'a> Interpreter<'a> {
    pub fn new(foreign_functions: &'a [ForeignFunction]) -> Self {
        Interpreter {
            frame_pointer: 0,
            pc: 0,
            stack: Vec::with_capacity(1000),
            foreign_functions,
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

    pub fn pop_stack_runtime_pointer(&mut self) -> InterpResult<usize> {
        match self.pop_stack() {
            Ok(val) => match val {
                Value::RuntimePointer(int) => Ok(int),
                _ => Err(InterpError::new(&format!(
                    "Stack value is not of type int {:?}",
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
        use self::InterpLogLevel::*;
        use self::OP::*;

        if self.log_level >= LogDebug {
            self.debug_log(
                LogDebug,
                &format!(
                    "PC: {} \t SP: {} \t Stack: {:?} \t Inst: {:?}",
                    self.pc, self.frame_pointer, self.stack, cmd
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
            CallForeign(addr) => {
                let function = &self.foreign_functions[*addr as usize];
                let function_call = &function.function;
                let mut args = self.get_foreign_function_arguments()?;
                if let Some(args_count) = function.arguments {
                    assert_eq!(args_count, args.len())
                }
                let return_values = function_call(&mut args)?;
                assert_eq!(function.returns, return_values.len())
            }
            PushPc(offset) => {
                self.push_stack(Value::RuntimePointer((offset + self.pc as isize) as usize))?;
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
                self.pc = self.pop_stack_runtime_pointer()?;
            }
            Jump(addr) => {
                self.pc = *addr;
            }
            PushStackFrame => {
                self.push_stack(Value::RuntimePointer(self.frame_pointer))?;
            }
            SetStackFrame(offset) => {
                let end = self.stack.len() as isize;
                self.frame_pointer = (end + *offset) as usize;
            }
            PopStackFrame => {
                self.frame_pointer = self.pop_stack_runtime_pointer()?;
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
                        "PC: {} \t SP: {} \t Inst: {:?}",
                        self.pc,
                        self.stack.len(),
                        cmd
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
