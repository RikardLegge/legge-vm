use crate::bytecode::{Bytecode, Instruction};
use crate::foreign_functions::ForeignFunction;

pub struct Interpreter<'a> {
    pc: usize,
    frame_pointer: usize,
    pub heap: Vec<i64>,
    pub stack: Vec<i64>,
    pub foreign_functions: &'a [ForeignFunction],
    pub log_level: InterpLogLevel,
    pub stack_max: usize,
}

#[allow(dead_code)]
#[derive(Debug, PartialOrd, PartialEq)]
pub enum InterpLogLevel {
    LogNone,
    LogDebug,
    LogEval,
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

pub type InterpResult = Result<(), InterpError>;
pub type ForeignInterpResult = Result<Vec<i64>, InterpError>;
pub type InterpPrimitiveResult = Result<i64, InterpError>;

impl<'a> Interpreter<'a> {
    pub fn new(foreign_functions: &'a [ForeignFunction]) -> Self {
        Interpreter {
            frame_pointer: 0,
            pc: 0,
            stack: Vec::with_capacity(1000),
            heap: Vec::with_capacity(1000),
            foreign_functions,
            log_level: InterpLogLevel::LogDebug,
            stack_max: 40,
        }
    }

    fn debug_log(&self, level: InterpLogLevel, info: &str) {
        if self.log_level >= level {
            println!("{}", info);
        }
    }

    pub fn pop_stack_count(&mut self, count: usize) -> InterpResult {
        if self.stack.len() >= count {
            for _ in 0..count {
                self.stack.pop();
            }
            Ok(())
        } else {
            Err(InterpError::new("Stack empty, can not pop"))
        }
    }

    pub fn pop_stack(&mut self) -> InterpPrimitiveResult {
        match self.stack.pop() {
            Some(val) => Ok(val),
            None => Err(InterpError::new("Stack empty, can not pop")),
        }
    }

    pub fn push_stack(&mut self, value: i64) -> InterpResult {
        if self.stack.len() < self.stack_max {
            self.stack.push(value);
            Ok(())
        } else {
            Err(InterpError::new("Stack overflow"))
        }
    }

    pub fn get_foreign_function_arguments(&mut self) -> Result<Vec<i64>, InterpError> {
        let count = self.pop_stack()? as usize;
        let mut arguments = Vec::with_capacity(count);
        for _ in 0..count {
            let argument = self.pop_stack()?;
            arguments.push(argument);
        }
        arguments.reverse();
        Ok(arguments)
    }

    fn run_command(&mut self, cmd: &Instruction, code: &Bytecode) -> InterpResult {
        use self::Instruction::*;
        use self::InterpLogLevel::*;

        self.debug_log(
            LogDebug,
            &format!(
                "PC: {} \t SP: {} \t Stack: {:?} \t Inst: {:?}",
                self.pc, self.frame_pointer, self.stack, cmd
            ),
        );
        self.pc += 1;
        match cmd {
            AddI => {
                let n1 = self.pop_stack()?;
                let n2 = self.pop_stack()?;
                let sum = n2 + n1;
                self.push_stack(sum)?;
                self.debug_log(LogEval, &format!("{} + {} = {}", n2, n1, sum));
            }
            SubI => {
                let n1 = self.pop_stack()?;
                let n2 = self.pop_stack()?;
                let diff = n2 - n1;
                self.push_stack(diff)?;
                self.debug_log(LogEval, &format!("{} - {} = {}", n2, n1, diff));
            }
            MulI => {
                let n1 = self.pop_stack()?;
                let n2 = self.pop_stack()?;
                let prod = n2 * n1;
                self.push_stack(prod)?;
                self.debug_log(LogEval, &format!("{} * {} = {}", n2, n1, prod));
            }
            DivI => {
                let n1 = self.pop_stack()?;
                let n2 = self.pop_stack()?;
                let kvote = n2 / n1;
                self.push_stack(kvote)?;
                self.debug_log(LogEval, &format!("{} / {} = {}", n2, n1, kvote));
            }
            EqI => {
                let n1 = self.pop_stack()?;
                let n2 = self.pop_stack()?;
                let eq = if n2 == n1 { 1 } else { 0 };
                self.push_stack(eq)?;
                self.debug_log(LogEval, &format!("({} == {}) = {}", n2, n1, eq));
            }
            SStore(offset) => {
                let value = self.pop_stack()?;
                let index = self.frame_pointer as i64 + *offset;
                assert!(index >= 0);
                self.stack[index as usize] = value;
            }
            SLoad(offset) => {
                let index = self.frame_pointer as i64 + *offset;
                let value = self.stack[index as usize];
                self.push_stack(value)?;
                self.debug_log(LogEval, &format!("{}", value));
            }
            PushImmediate(primitive, _) => self.push_stack(*primitive)?,
            PopStack(count) => {
                self.pop_stack_count(*count)?;
            }
            CallForeign(addr, _) => {
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
                self.push_stack((offset + self.pc) as i64)?;
            }
            Branch(pc) => {
                self.pc = (self.pc as isize + *pc) as usize;
            }
            BranchIf(pc) => {
                let value = self.pop_stack()?;
                if value != 0 {
                    self.pc = (self.pc as isize + *pc) as usize;
                }
            }
            PopPc => {
                self.pc = self.pop_stack()? as usize;
            }
            Call(addr, _) => {
                self.pc = code.procedure_address + *addr;
            }
            PushStackFrame => {
                self.push_stack(self.frame_pointer as i64)?;
            }
            SetStackFrame(offset) => {
                let end = self.stack.len() as i64;
                self.frame_pointer = (end + *offset) as usize;
            }
            PopStackFrame => {
                self.frame_pointer = self.pop_stack()? as usize;
            }
            NoOp => {}
            _ => panic!("Instruction not implemented: {:?}", cmd),
        }
        Ok(())
    }

    fn setup(&mut self, code: &Bytecode) {
        self.pc = 0;
        self.frame_pointer = 0;
        self.stack.clear();
        self.heap.clear();
        self.heap.append(&mut code.data.clone());
    }

    pub fn run(&mut self, code: &Bytecode) {
        self.setup(code);

        let mut i = 10000;

        while let Some(cmd) = code.code.get(self.pc) {
            if i == 0 {
                break;
            }
            i -= 1;
            if cmd == &Instruction::Halt {
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
            if let Err(err) = self.run_command(cmd, code) {
                panic!("{:?}", err);
            }
        }
        dbg!(&self.stack);
    }
}
