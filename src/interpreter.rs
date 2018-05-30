use bytecode::{Bytecode, Instruction, Data};
use ast::AstPrimitives;

#[derive(Debug)]
pub struct Interpreter {
    program_counter: usize,
    link_register: usize,
    pub stack: Vec<AstPrimitives>,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter { link_register: 0, program_counter: 0, stack: Vec::new() }
    }

    fn run_command(&mut self, cmd: &Instruction, code: &Bytecode) -> Option<()> {
        self.program_counter += 1;
        match cmd {
            Instruction::AddI => {
                let prim1 = self.stack.pop()?;
                let prim2 = self.stack.pop()?;

                match (prim1, prim2) {
                    (AstPrimitives::Int(num1), AstPrimitives::Int(num2)) => {
                        let res = AstPrimitives::Int(num1 + num2);
                        self.stack.push(res);
                    },
                    (_,_) => panic!("NANANA"),
                }
            }
            Instruction::Push(primitive) => self.stack.push(primitive.clone()),
            Instruction::BlFFI(addr) => {
                self.link_register = self.program_counter;
                match code.data[*addr as usize] {
                    Data::FFiFunction(_, ref func) => {
                        self.run_command(code.code.get(self.program_counter)?, code);
                        if let &AstPrimitives::Int(count) = self.stack.last()? {
                            for _ in 0..count {
                                self.run_command(code.code.get(self.program_counter)?, code);
                            }
                            let to_call = func.function;
                            to_call(self, code);
                        }
                    },
                    _ => panic!("NOOO")
                }
            }
        }
        None
    }

    pub fn run(&mut self, code: &Bytecode) -> Option<()> {
        while let Some(cmd) = code.code.get(self.program_counter) {
            self.run_command(cmd, code);
        }
        println!("{:?}", self.stack);
        Some(())
    }
}