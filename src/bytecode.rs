use crate::ast::{Ast, AstNode, AstNodeBody, NodeID, VariableType};
use crate::bytecode::OP::Branch;
use crate::foreign_functions::ForeignFunction;
use crate::token::ArithmeticOP;
use serde::{Deserialize, Serialize};
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
            bc.ev_node(&ast.root).stack_usage
        );
        let global_scope = mem::replace(&mut bc.scope, Scope::new(0));
        assert_eq!(bc.code.len(), 0);
        bc.code = global_scope.instructions;
        // bc.optimize();
        bc.get_bytecode()
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Instruction {
    pub node_id: NodeID,
    pub op: OP,
}

#[derive(Debug)]
struct GeneratorInstruction {
    node_id: NodeID,
    op: GeneratorOP,
}

#[derive(Debug)]
enum GeneratorOP {
    Linked(OP),
    Unlinked(UnlinkedOP),
}

pub type OPOffset = isize;
pub type OPAddress = usize;

#[derive(Debug)]
enum UnlinkedOP {
    Panic(String),
    Branch(LinkTarget),
    BranchIf(LinkTarget),
    SLoad(String),
    SStore(String),
    PopStack(NodeID, NodeID),
    Call(String),
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum OP {
    AddI,
    SubI,
    MulI,
    DivI,
    CmpI,

    SLoad(OPOffset),
    SStore(OPOffset),

    Branch(OPOffset),
    BranchIf(OPOffset),
    PushPc(OPOffset),
    PopPc,

    PushImmediate(Value),
    PopStack(usize),

    SetStackFrame(OPOffset),
    PushStackFrame,
    PopStackFrame,

    Jump(OPAddress),
    CallForeign(OPAddress),

    NoOp,
    Halt,
    Panic,
}
#[derive(Copy, Clone, Debug, PartialEq, Serialize, Deserialize)]
enum LinkTarget {
    If(NodeID, LinkTargetPosition),
    Loop(NodeID, LinkTargetPosition),
    Return(NodeID, LinkTargetPosition),
}

#[derive(Copy, Clone, Debug, PartialEq, Serialize, Deserialize)]
enum LinkTargetPosition {
    Start,
    Else,
    End,
}

#[derive(Debug, PartialEq, Serialize, Deserialize, Clone)]
pub enum Value {
    Unset,
    Int(isize),
    FunctionPointer(usize),
    RuntimePointer(usize),
}

#[derive(Clone, Debug, PartialEq)]
enum ScopeValue {
    Expression,
    StackFrame,
    Body,
    Variable(String, bool),
    Block,
    Loop,
}

struct Scope {
    node_id: NodeID,
    parent: Option<Box<Scope>>,
    value: ScopeValue,
    instructions: Vec<GeneratorInstruction>,
}

impl Scope {
    fn new(node_id: NodeID) -> Self {
        Scope {
            node_id,
            parent: None,
            instructions: Vec::new(),
            value: ScopeValue::Block,
        }
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
    node_id: NodeID,
    code: Vec<GeneratorInstruction>,
    heap: Vec<i64>,
    scope: Scope,
    procedures: Vec<GeneratorInstruction>,
    foreign_functions: &'a [ForeignFunction],
}

struct ScopeEvaluation {
    stack_usage: StackUsage,
    scope: Scope,
}

impl<'a> BytecodeGenerator<'a> {
    fn get_bytecode(self) -> Bytecode {
        let data = self.heap;

        let mut generator_code = self.code;
        generator_code.push(GeneratorInstruction {
            node_id: 0,
            op: GeneratorOP::Linked(OP::Halt),
        });
        let procedure_address = generator_code.len();

        let mut procedures = self.procedures;
        generator_code.append(&mut procedures);

        let mut code = Vec::with_capacity(generator_code.len());
        for inst in generator_code {
            let op = match inst.op {
                GeneratorOP::Linked(op) => op,
                GeneratorOP::Unlinked(op) => panic!(
                    "Found Unlinked instruction after bytecode generation completed: {:?}",
                    op
                ),
            };
            let node_id = inst.node_id;
            code.push(Instruction { node_id, op });
        }

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
            node_id: 0,
            code,
            scope,
            foreign_functions,
            heap,
            procedures,
        };
        for (_, function) in foreign_functions.iter().enumerate() {
            gen.push_scope(0, ScopeValue::Variable(function.name.clone().into(), true));
        }
        gen
    }

    fn add_child_ops(&mut self, scope: &mut Scope) {
        self.scope.instructions.append(&mut scope.instructions);
    }

    fn add_op(&mut self, op: OP) {
        let linked_op = GeneratorOP::Linked(op);
        self.scope.instructions.push(GeneratorInstruction {
            node_id: self.node_id,
            op: linked_op,
        });
    }

    fn add_op_unlinked(&mut self, op: UnlinkedOP) {
        let unlinked_op = GeneratorOP::Unlinked(op);
        self.scope.instructions.push(GeneratorInstruction {
            node_id: self.node_id,
            op: unlinked_op,
        });
    }

    fn with_scope<'b>(
        &mut self,
        node_id: NodeID,
        value: ScopeValue,
        func: &'b dyn Fn(&mut Self) -> StackUsage,
    ) -> ScopeEvaluation {
        self.push_scope(node_id, value.clone());
        let stack_usage = func(self);
        let scope = self.pop_scope(node_id, value);
        return ScopeEvaluation { stack_usage, scope };
    }

    fn push_scope(&mut self, node_id: NodeID, value: ScopeValue) {
        let parent = mem::replace(&mut self.scope, Scope::new(node_id));
        self.scope.parent = Some(Box::new(parent));
        self.scope.value = value;
    }

    fn pop_scope(&mut self, node_id: NodeID, value: ScopeValue) -> Scope {
        assert_eq!(self.scope.node_id, node_id);
        assert_eq!(self.scope.value, value);
        self.pop_top_scope()
    }

    fn pop_top_scope(&mut self) -> Scope {
        let parent = *mem::replace(&mut self.scope.parent, None).unwrap();
        mem::replace(&mut self.scope, parent)
    }

    fn ev_node(&mut self, node: &AstNode) -> ScopeEvaluation {
        use crate::ast::AstNodeBody::*;
        match &node.body {
            Op(op, expr1, expr2) => self.ev_operation(node.id, *op, &expr1, &expr2),
            PrefixOp(op, expr1) => self.ev_prefix_operation(node.id, *op, &expr1),
            Block(children) => self.ev_block(node.id, children),
            Call(name, args) => self.ev_call(node.id, &name, args),
            VariableValue(name) => self.ev_variable_value(node.id, &name),
            VariableAssignment(tp, name, expr1) => match tp {
                VariableType::Assignment => self.ev_assignment(node.id, &name, expr1),
                VariableType::Declaration => self.ev_declaration(node.id, &name, expr1, false),
                VariableType::ConstDeclaration => self.ev_declaration(node.id, &name, expr1, true),
            },
            ProcedureDeclaration(args, return_values, body) => {
                self.ev_procedure(node.id, &args, &return_values, body)
            }
            Return(proc_id, _) => self.ev_return(node.id, *proc_id),
            If(condition, body) => self.ev_if(node.id, condition, body),
            Loop(body) => self.ev_loop(node.id, body),
            Break(id) => self.ev_break(node.id, *id),
            Comment(_) => ScopeEvaluation {
                stack_usage: StackUsage::new(0, 0),
                scope: Scope::new(node.id),
            },
            _ => panic!("Unsupported node here {:?}", node),
        }
    }

    fn ev_break(&mut self, node_id: NodeID, loop_id: NodeID) -> ScopeEvaluation {
        self.with_scope(node_id, ScopeValue::Expression, &|bc| {
            bc.add_op_unlinked(UnlinkedOP::PopStack(node_id, loop_id));
            bc.add_op_unlinked(UnlinkedOP::Branch(LinkTarget::Loop(
                loop_id,
                LinkTargetPosition::End,
            )));
            StackUsage::new(0, 0)
        })
    }

    fn ev_loop(&mut self, node_id: NodeID, body_node: &Box<AstNode>) -> ScopeEvaluation {
        self.with_scope(node_id, ScopeValue::Loop, &|bc| {
            let start = bc.scope.instructions.len();
            let mut body = match &body_node.body {
                AstNodeBody::Block(children) => {
                    let body = bc.ev_block(body_node.id, children);
                    let usage = body.stack_usage;
                    assert_eq!(usage.pushed, usage.popped);
                    body.scope
                }
                _ => panic!("The body of a loop statement must be a scope"),
            };
            let end = bc.scope.instructions.len();
            let start_of_loop = start as isize - end as isize - 1;
            bc.add_op(OP::Branch(start_of_loop));

            for i in 0..body.instructions.len() {
                match &body.instructions[i].op {
                    GeneratorOP::Unlinked(UnlinkedOP::Branch(LinkTarget::Loop(id, target)))
                        if *id == node_id =>
                    {
                        body.instructions[i].op = match target {
                            LinkTargetPosition::Start => {
                                GeneratorOP::Linked(Branch(-(i as isize) - 1))
                            }
                            LinkTargetPosition::End => GeneratorOP::Linked(Branch(
                                (body.instructions.len() - i) as isize - 1,
                            )),
                            _ => panic!("Unable to link loop return to else"),
                        }
                    }
                    _ => {}
                }
            }

            bc.scope.instructions.append(&mut body.instructions);

            StackUsage {
                popped: 0,
                pushed: 0,
            }
        })
    }

    fn ev_if(
        &mut self,
        node_id: NodeID,
        condition: &Box<AstNode>,
        node: &Box<AstNode>,
    ) -> ScopeEvaluation {
        self.with_scope(node_id, ScopeValue::Expression, &|bc| {
            let usage = bc.ev_expression(condition).stack_usage;
            assert_eq!(usage.pushed - usage.popped, 1);
            bc.add_op(OP::PushImmediate(Value::Unset));
            bc.add_op(OP::CmpI);
            let jump_index = bc.scope.instructions.len();
            bc.add_op_unlinked(UnlinkedOP::BranchIf(LinkTarget::If(
                0,
                LinkTargetPosition::Else,
            )));

            let start = bc.scope.instructions.len();
            if let AstNodeBody::Block(children) = &node.body {
                bc.ev_block(node.id, children);
            } else {
                panic!("The body of an if statement must be a Block");
            }
            let end = bc.scope.instructions.len();
            let end_of_if = (end - start) as isize;
            bc.scope.instructions[jump_index].op = GeneratorOP::Linked(OP::BranchIf(end_of_if));

            StackUsage {
                popped: 0,
                pushed: 0,
            }
        })
    }

    fn ev_return(&mut self, node_id: NodeID, proc_id: NodeID) -> ScopeEvaluation {
        self.with_scope(node_id, ScopeValue::Expression, &|bc| {
            bc.add_op_unlinked(UnlinkedOP::PopStack(node_id, proc_id));
            bc.add_op(OP::PopStackFrame);
            bc.add_op(OP::PopPc);
            StackUsage::new(0, 0)
        })
    }

    fn ev_procedure(
        &mut self,
        node_id: NodeID,
        args: &[String],
        _: &Option<String>,
        children: &[AstNode],
    ) -> ScopeEvaluation {
        self.with_scope(node_id, ScopeValue::Expression, &|bc| {
            let mut body = bc.with_scope(node_id, ScopeValue::StackFrame, &|bc| {
                for arg in args.iter() {
                    bc.push_scope(node_id, ScopeValue::Variable(arg.into(), true));
                }
                let usage = bc.ev_block(node_id, children).stack_usage;
                assert_eq!(usage.pushed, usage.popped);

                if let Some(inst) = bc.scope.instructions.last() {
                    if let GeneratorOP::Linked(OP::PopPc) = inst.op {
                        bc.ev_return(node_id, node_id);
                    }
                }
                StackUsage::new(0, 0)
            });

            let proc_offset = bc.procedures.len();
            bc.procedures.append(&mut body.scope.instructions);

            let value = Value::FunctionPointer(proc_offset);
            bc.add_op(OP::PushImmediate(value));

            StackUsage::new(0, 1)
        })
    }

    fn ev_variable_value(&mut self, node_id: NodeID, symbol: &str) -> ScopeEvaluation {
        self.with_scope(node_id, ScopeValue::Expression, &|bc| {
            bc.add_op_unlinked(UnlinkedOP::SLoad(symbol.into()));
            StackUsage::new(0, 1)
        })
    }

    fn ev_assignment(&mut self, node_id: NodeID, symbol: &str, expr: &AstNode) -> ScopeEvaluation {
        self.with_scope(node_id, ScopeValue::Expression, &|bc| {
            let usage = match expr.body {
                AstNodeBody::Int(val) => bc.ev_intermediate(expr.id, Value::Int(val)),
                _ => bc.ev_node(expr),
            }
            .stack_usage;
            assert_eq!(usage.pushed, 1);

            bc.add_op_unlinked(UnlinkedOP::SStore(symbol.into()));
            StackUsage::new(usage.popped, 0)
        })
    }

    fn ev_declaration(
        &mut self,
        node_id: NodeID,
        symbol: &str,
        expr: &AstNode,
        is_constant: bool,
    ) -> ScopeEvaluation {
        let value = ScopeValue::Variable(symbol.into(), is_constant);
        self.with_scope(node_id, value, &|bc| {
            bc.ev_assignment(node_id, symbol, expr).stack_usage
        })
    }

    fn ev_call(&mut self, node_id: NodeID, symbol: &str, args: &[AstNode]) -> ScopeEvaluation {
        self.with_scope(node_id, ScopeValue::Expression, &|bc| {
            let pc_index = bc.scope.instructions.len();
            bc.add_op_unlinked(UnlinkedOP::Panic(
                "PushPC instruction offset not written correctly".into(),
            ));
            bc.add_op(OP::PushStackFrame);
            let mut popped = 0;
            for arg in args {
                let usage = bc.ev_expression(arg).stack_usage;
                assert_eq!(1, usage.pushed);
                popped += usage.popped;
            }

            bc.add_op(OP::SetStackFrame(-(args.len() as isize)));
            bc.add_op_unlinked(UnlinkedOP::Call(symbol.into()));
            let offset = bc.scope.instructions.len() - pc_index - 1;
            bc.scope.instructions[pc_index].op = GeneratorOP::Linked(OP::PushPc(offset as isize));

            StackUsage::new(popped, 0)
        })
    }

    fn ev_block(&mut self, node_id: NodeID, children: &[AstNode]) -> ScopeEvaluation {
        self.with_scope(node_id, ScopeValue::Block, &|bc| {
            bc.push_scope(node_id, ScopeValue::Body);
            let mut stack_usage = StackUsage::new(0, 0);
            for node in children {
                stack_usage += bc.ev_node(node).stack_usage;
                if stack_usage.popped > stack_usage.pushed {
                    panic!("More elements have been popped than pushed");
                }
            }
            assert_eq!(stack_usage.pushed, stack_usage.popped);

            let mut allocations = 0;
            while bc.scope.node_id != node_id {
                let scope = bc.pop_top_scope();
                match scope.value {
                    ScopeValue::Variable(_, _) => (),
                    _ => panic!(
                        "Invalid scope type when calculating block allocations {:?}",
                        scope.value
                    ),
                }

                bc.add_op(OP::PushImmediate(Value::Unset));
                allocations += 1;
            }
            let mut body = bc.pop_scope(node_id, ScopeValue::Body);

            bc.add_child_ops(&mut body);
            bc.add_op(OP::PopStack(allocations));
            return StackUsage::new(0, 0);
        })
    }

    fn ev_prefix_operation(
        &mut self,
        node_id: NodeID,
        op: ArithmeticOP,
        expr1: &AstNode,
    ) -> ScopeEvaluation {
        self.with_scope(node_id, ScopeValue::Expression, &|bc| {
            bc.add_op(OP::PushImmediate(Value::Int(0)));
            let usage = bc.ev_expression(expr1).stack_usage;
            assert_eq!(1, usage.pushed);
            match op {
                ArithmeticOP::Add => bc.add_op(OP::AddI),
                ArithmeticOP::Sub => bc.add_op(OP::SubI),
                _ => panic!("Invalid prefix operation: {:?}", op),
            };
            StackUsage::new(usage.popped, 1)
        })
    }

    fn ev_operation(
        &mut self,
        node_id: NodeID,
        op: ArithmeticOP,
        expr1: &AstNode,
        expr2: &AstNode,
    ) -> ScopeEvaluation {
        self.with_scope(node_id, ScopeValue::Expression, &|bc| {
            let usage1 = bc.ev_expression(expr1).stack_usage;
            assert_eq!(usage1.pushed, 1);
            let usage2 = bc.ev_expression(expr2).stack_usage;
            assert_eq!(usage2.pushed, 1);

            match op {
                ArithmeticOP::Add => bc.add_op(OP::AddI),
                ArithmeticOP::Sub => bc.add_op(OP::SubI),
                ArithmeticOP::Mul => bc.add_op(OP::MulI),
                ArithmeticOP::Div => bc.add_op(OP::DivI),
                ArithmeticOP::Eq => bc.add_op(OP::CmpI),
            };
            StackUsage::new(usage1.popped + usage2.popped, 1)
        })
    }

    fn ev_expression(&mut self, expr: &AstNode) -> ScopeEvaluation {
        use crate::ast::AstNodeBody::*;
        match &expr.body {
            Op(op, expr1, expr2) => self.ev_operation(expr.id, *op, &expr1, &expr2),
            Int(primitive) => self.ev_intermediate(expr.id, Value::Int(*primitive)),
            VariableValue(symbol) => self.ev_variable_value(expr.id, symbol),
            Call(symbol, args) => self.ev_call(expr.id, symbol, args),
            _ => panic!("Unsupported node {:?} used as expression", expr),
        }
    }

    fn ev_intermediate(&mut self, node_id: NodeID, value: Value) -> ScopeEvaluation {
        self.with_scope(node_id, ScopeValue::Expression, &|bc| {
            bc.add_op(OP::PushImmediate(value.clone()));
            StackUsage::new(0, 1)
        })
    }
}
