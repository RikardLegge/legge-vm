use bytecode::{Bytecode, Instruction, Data};

#[derive(Debug)]
pub struct Interpreter {
    program_counter: usize,
    link_register: usize,
    pub stack: Vec<i64>,
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

impl Interpreter {
    pub fn new() -> Self {
        Interpreter { link_register: 0, program_counter: 0, stack: Vec::new() }
    }

    pub fn pop_stack(&mut self) -> InterpPrimitiveResult {
        match self.stack.pop() {
            Some(val) => Ok(val),
            None => Err(InterpError::new("Stack empty, can not pop"))
        }
    }

    pub fn peek_stack(&mut self) -> InterpPrimitiveResult {
        match self.stack.last() {
            Some(val) => Ok(*val),
            None => Err(InterpError::new("Stack empty, can not pop"))
        }
    }

    fn get_instruction<'a>(&self, code: &'a Bytecode) -> InterpInstructionResult<'a> {
        match code.code.get(self.program_counter) {
            Some(val) => Ok(val),
            None => Err(InterpError::new("Can not fetch next instruction"))
        }
    }

    fn run_command(&mut self, cmd: &Instruction, code: &Bytecode) -> InterpResult {
        self.program_counter += 1;
        match cmd {
            Instruction::AddI => {
                let n1 = self.pop_stack()?;
                let n2 = self.pop_stack()?;
                self.stack.push(n2 + n1);
            }
            Instruction::SubI => {
                let n1 = self.pop_stack()?;
                let n2 = self.pop_stack()?;
                self.stack.push(n2 - n1);
            }
            Instruction::MulI => {
                let n1 = self.pop_stack()?;
                let n2 = self.pop_stack()?;
                self.stack.push(n2 * n1);
            }
            Instruction::DivI => {
                let n1 = self.pop_stack()?;
                let n2 = self.pop_stack()?;
                self.stack.push(n2 / n1);
            }
            Instruction::PushLoad(addr) => {
                match code.data[*addr] {
                    Data::Constant(_, value) => self.stack.push(value),
                    _ => panic!("Invalid data type")
                }
            }
            Instruction::PushImmediate(primitive) => self.stack.push(*primitive),
            Instruction::BlFFI(addr) => {
                self.link_register = self.program_counter;
                match code.data[*addr as usize] {
                    Data::FFiFunction(_, ref func) => {
                        let to_call = func.function;
                        to_call(self, code)?;
                    }
                    _ => panic!("NOOO")
                }
            }
        }
        Ok(())
    }

    pub fn run(&mut self, code: &Bytecode) {
        while let Some(cmd) = code.code.get(self.program_counter) {
            if let Err(err) = self.run_command(cmd, code) {
                panic!("{:?}", err);
            }
        }
        println!("{:?}", self.stack);
    }
}