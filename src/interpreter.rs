use bytecode::{Bytecode, Instruction};
use foreign_functions::ForeignFunction;

pub struct Interpreter<'a> {
    program_counter: usize,
    frame_pointer: usize,
    pub heap: Vec<i64>,
    pub stack: Vec<i64>,
    pub foreign_functions: &'a [ForeignFunction]
}

#[derive(Debug)]
pub struct InterpError {
    details: String
}

impl InterpError {
    pub fn new(details: &str) -> InterpError {
        InterpError { details: details.to_string() }
    }
}

pub type InterpResult = Result<(), InterpError>;
pub type InterpPrimitiveResult = Result<i64, InterpError>;
pub type InterpInstructionResult<'a> = Result<&'a Instruction, InterpError>;

impl<'a> Interpreter<'a> {
    pub fn new(foreign_functions: &'a [ForeignFunction]) -> Self {
        Interpreter {
            frame_pointer: 0,
            program_counter: 0,
            stack: Vec::with_capacity(1000),
            heap: Vec::with_capacity(10000),
            foreign_functions
        }
    }

    pub fn pop_stack(&mut self) -> InterpPrimitiveResult {
        match self.stack.pop() {
            Some(val) => Ok(val),
            None => Err(InterpError::new("Stack empty, can not pop"))
        }
    }

    pub fn push_stack(&mut self, value: i64) {
        self.stack.push(value);
    }

    pub fn peek_stack(&mut self) -> InterpPrimitiveResult {
        match self.stack.last() {
            Some(val) => Ok(*val),
            None => Err(InterpError::new("Stack empty, can not peek"))
        }
    }

    pub fn get_foreign_function_arguments(&mut self) -> Result<Vec<i64>, InterpError> {
        let mut arguments = Vec::new();
        let count = self.pop_stack()?;
        for _ in 0..count {
            let argument = self.pop_stack()?;
            arguments.push(argument);
        }
        Ok(arguments)
    }

    fn get_instruction<'b>(&self, code: &'b Bytecode) -> InterpInstructionResult<'b> {
        match code.code.get(self.program_counter) {
            Some(val) => Ok(val),
            None => Err(InterpError::new("Can not fetch next instruction"))
        }
    }

    fn run_command(&mut self, cmd: &Instruction, code: &Bytecode) -> InterpResult {
        use self::Instruction::*;
        
        self.program_counter += 1;
        match cmd {
            AddI => {
                let n1 = self.pop_stack()?;
                let n2 = self.pop_stack()?;
                self.stack.push(n2 + n1);
            }
            SubI => {
                let n1 = self.pop_stack()?;
                let n2 = self.pop_stack()?;
                self.stack.push(n2 - n1);
            }
            MulI => {
                let n1 = self.pop_stack()?;
                let n2 = self.pop_stack()?;
                self.stack.push(n2 * n1);
            }
            DivI => {
                let n1 = self.pop_stack()?;
                let n2 = self.pop_stack()?;
                self.stack.push(n2 / n1);
            }
            SStore(offset) => {
                let value = self.pop_stack()?;
                self.stack[self.frame_pointer + offset] = value;
            }
            SLoad(offset) => {
                let value = self.stack[self.frame_pointer + offset];
                self.stack.push(value);
            }
            PushImmediate(primitive) => self.stack.push(*primitive),
            Pop => {self.pop_stack()?;}
            CallForeign(addr) => {
                let function =&self.foreign_functions[*addr as usize].function;
                function(self, code)?;
            }
            _ => panic!("Instruction not implemented: {:?}", cmd)
        }
        Ok(())
    }

    fn setup(&mut self, code: &Bytecode) {
        self.program_counter = 0;
        self.frame_pointer = 0;
        self.stack.clear();
        self.heap.clear();
        self.heap.append(&mut code.data.clone());
    }

    pub fn run(&mut self, code: &Bytecode) {
        self.setup(code);

        while let Some(cmd) = code.code.get(self.program_counter) {
            if let Err(err) = self.run_command(cmd, code) {
                panic!("{:?}", err);
            }
        }
        println!("{:?}", self.stack);
    }
}