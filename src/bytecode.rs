use crate::ast::{Ast, NodeBody, NodeID, NodeValue};
use crate::token::ArithmeticOP;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::ops::AddAssign;

pub fn from_ast(ast: &Ast) -> Bytecode {
    let root_id = ast.root();
    let mut bc = BytecodeGenerator::new(ast, root_id);
    assert_eq!(StackUsage::zero(), bc.ev_node(root_id));
    bc.get_bytecode()
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Bytecode {
    pub procedure_address: usize,
    pub code: Vec<Instruction>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Instruction {
    pub node_id: NodeID,
    pub op: OP,
}

#[derive(Debug)]
struct GeneratorInstruction {
    node_id: NodeID,
    op: OP,
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

    Call(OPAddress),

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
enum ContextType {
    StackFrame,
    Block,
    Loop { break_inst: usize },
}

struct ContextVariable {
    node_id: NodeID,
    offset: usize,
}

impl ContextVariable {
    fn new(node_id: NodeID, offset: usize) -> Self {
        Self { node_id, offset }
    }
}

struct Context {
    node_id: NodeID,
    variables: Vec<ContextVariable>,
    tp: ContextType,
}

impl Context {
    fn new(node_id: NodeID, tp: ContextType) -> Self {
        let variables = Vec::new();
        Self {
            node_id,
            tp,
            variables,
        }
    }
}

struct Scope {
    node_id: NodeID,
    instructions: Vec<Instruction>,
}

impl Scope {
    fn new(node_id: NodeID) -> Self {
        Scope {
            node_id,
            instructions: Vec::new(),
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

    fn zero() -> Self {
        StackUsage {
            popped: 0,
            pushed: 0,
        }
    }
}

struct Procedure {
    scope: Scope,
    address: usize,
}

struct BytecodeGenerator<'a> {
    procedures: HashMap<NodeID, Procedure>,
    ast: &'a Ast,
    scopes: Vec<Scope>,
    contexts: Vec<Context>,
}

impl<'a> BytecodeGenerator<'a> {
    fn get_bytecode(mut self) -> Bytecode {
        assert_eq!(1, self.scopes.len());
        let scope = self.scopes.pop().unwrap();
        let mut code = scope.instructions;
        code.push(Instruction {
            node_id: self.ast.root(),
            op: OP::Halt,
        });

        let procedure_address = code.len();
        for (_, mut proc) in self.procedures {
            code.append(&mut proc.scope.instructions);
        }

        Bytecode {
            code,
            procedure_address,
        }
    }

    fn new(ast: &'a Ast, node_id: NodeID) -> Self {
        let procedures = HashMap::new();
        let instruction_scope = vec![Scope::new(node_id)];
        let context = vec![Context::new(node_id, ContextType::Block)];
        BytecodeGenerator {
            scopes: instruction_scope,
            procedures,
            ast,
            contexts: context,
        }
    }

    fn get_variable_offset(&self, var_id: NodeID) -> OPOffset {
        for context in self.contexts.iter().rev() {
            if let Some(var) = context.variables.iter().find(|var| var.node_id == var_id) {
                return var.offset as isize;
            }
        }
        panic!(
            "Failed to get variable offset of {:?}",
            self.ast.get_node(var_id)
        );
    }

    fn get_proc_address(&self, proc_id: NodeID) -> OPAddress {
        match self.procedures.get(&proc_id) {
            Some(proc) => proc.address,
            None => panic!(
                "Failed to find procedure in scope {:?}",
                self.ast.get_node(proc_id)
            ),
        }
    }

    fn get_loop_break_offset(&self, loop_id: NodeID) -> OPOffset {
        for context in self.contexts.iter().rev() {
            if context.node_id == loop_id {
                match context.tp {
                    ContextType::Loop { break_inst } => {
                        return break_inst as isize - self.op_index() as isize - 1;
                    }
                    _ => unreachable!(),
                }
            }
        }
        panic!(
            "Failed to find loop searching for {:?}",
            self.ast.get_node(loop_id)
        );
    }
    fn get_allocations(&self, node_id: NodeID) -> usize {
        let mut allocations = 0;
        for context in self.contexts.iter().rev() {
            allocations += context.variables.len();
            if context.node_id == node_id {
                return allocations;
            }
        }
        panic!(
            "Failed to find node when counting allocations: {:?}",
            self.ast.get_node(node_id)
        );
    }

    fn get_scope(&self) -> &Scope {
        self.scopes.last().unwrap()
    }

    fn get_scope_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }

    fn get_context(&self) -> &Context {
        self.contexts.last().unwrap()
    }

    fn get_context_mut(&mut self) -> &mut Context {
        self.contexts.last_mut().unwrap()
    }

    fn add_child_ops(&mut self, scope: &mut Scope) {
        self.get_scope_mut()
            .instructions
            .append(&mut scope.instructions);
    }

    fn op_index(&self) -> usize {
        self.get_scope().instructions.len()
    }

    fn add_var(&mut self, var_id: NodeID) {
        let mut offset = 0;
        for context in self.contexts.iter().rev() {
            offset += context.variables.len();
            if context.tp == ContextType::StackFrame {
                break;
            }
        }

        let context = self.get_context_mut();
        let var = ContextVariable::new(var_id, offset);
        context.variables.push(var);
    }

    fn add_proc(&mut self, scope: Scope) -> OPAddress {
        let address = self
            .procedures
            .iter()
            .map(|(_, p)| p.scope.instructions.len())
            .sum();
        let proc = Procedure { scope, address };
        self.procedures.insert(proc.scope.node_id, proc);
        address
    }

    fn add_op(&mut self, node_id: NodeID, op: OP) -> usize {
        let instruction = Instruction { node_id, op };
        let i = self.op_index();
        self.get_scope_mut().instructions.push(instruction);
        i
    }

    fn set_op(&mut self, index: usize, op: OP) {
        self.get_scope_mut().instructions[index].op = op
    }

    fn with_context<F, T>(&mut self, node_id: NodeID, tp: ContextType, func: F) -> T
    where
        F: Fn(&mut Self) -> T,
    {
        self.push_context(node_id, tp);
        let result = func(self);
        self.pop_context(node_id);
        result
    }

    fn with_scope<F>(&mut self, node_id: NodeID, tp: ContextType, func: F) -> Scope
    where
        F: Fn(&mut Self),
    {
        self.push_scope(node_id);
        self.with_context(node_id, tp, func);
        self.pop_scope(node_id)
    }

    fn push_context(&mut self, node_id: NodeID, tp: ContextType) {
        self.contexts.push(Context::new(node_id, tp));
    }

    fn pop_context(&mut self, node_id: NodeID) {
        let context = self.contexts.pop().unwrap();
        assert_eq!(context.node_id, node_id);
    }

    fn push_scope(&mut self, node_id: NodeID) {
        self.scopes.push(Scope::new(node_id));
    }

    fn pop_scope(&mut self, node_id: NodeID) -> Scope {
        let scope = self.scopes.pop().unwrap();
        assert_eq!(scope.node_id, node_id);
        scope
    }

    fn ev_node(&mut self, node_id: NodeID) -> StackUsage {
        use crate::ast::NodeBody::*;
        let node = self.ast.get_node(node_id);
        match &node.body {
            Op(op, expr1, expr2) => self.ev_operation(node_id, *op, *expr1, *expr2),
            PrefixOp(op, expr1) => self.ev_prefix_operation(node_id, *op, *expr1),
            Block(children) => self.ev_block(node_id, children),
            Call(proc_id, args) => self.ev_call(node_id, *proc_id, args),
            VariableValue(val) => self.ev_variable_value(node_id, *val),
            ConstDeclaration(_, _, expr) => self.ev_declaration(node_id, Some(*expr)),
            VariableDeclaration(_, _, expr) => self.ev_declaration(node_id, *expr),
            VariableAssignment(var, value) => self.ev_assignment(node_id, *var, *value),
            ProcedureDeclaration(args, returns, body) => {
                self.ev_procedure(node_id, &args, returns, *body)
            }
            Return(proc_id) => self.ev_return(node_id, *proc_id),
            If(condition, body) => self.ev_if(node_id, *condition, *body),
            Loop(body) => self.ev_loop(node_id, *body),
            Break(loop_id) => self.ev_break(node_id, *loop_id),
            Comment(_) => StackUsage::zero(),
            _ => panic!("Unsupported node here {:?}", node),
        }
    }

    fn ev_break(&mut self, node_id: NodeID, loop_id: NodeID) -> StackUsage {
        let allocations = self.get_allocations(loop_id);
        self.add_op(node_id, OP::PopStack(allocations));
        let offset = self.get_loop_break_offset(loop_id);
        self.add_op(node_id, OP::Branch(offset));
        StackUsage::new(0, 0)
    }

    fn ev_loop(&mut self, node_id: NodeID, body_id: NodeID) -> StackUsage {
        self.add_op(node_id, OP::Branch(1));
        let break_inst = self.add_op(node_id, OP::Panic);
        let start = self.op_index();
        self.with_context(node_id, ContextType::Loop { break_inst }, |bc| {
            let body_node = bc.ast.get_node(body_id);
            match &body_node.body {
                NodeBody::Block(children) => {
                    let usage = bc.ev_block(body_id, children);
                    assert_eq!(usage.pushed, usage.popped);
                }
                _ => panic!("The body of a loop statement must be a scope"),
            };
        });
        let end = self.op_index();
        self.add_op(node_id, OP::Branch(start as isize - end as isize - 1));
        self.set_op(break_inst, OP::Branch(end as isize - start as isize));
        StackUsage {
            popped: 0,
            pushed: 0,
        }
    }

    fn ev_if(&mut self, node_id: NodeID, condition_id: NodeID, body_id: NodeID) -> StackUsage {
        let usage = self.ev_expression(condition_id);
        assert_eq!(usage.pushed - usage.popped, 1);
        self.add_op(node_id, OP::PushImmediate(Value::Unset));
        self.add_op(node_id, OP::CmpI);
        let jump_index = self.add_op(node_id, OP::Panic);

        let start = self.op_index();
        let body_node = self.ast.get_node(body_id);
        match &body_node.body {
            NodeBody::Block(children) => {
                let usage = self.ev_block(body_id, children);
                assert_eq!(usage.pushed, usage.popped);
            }
            _ => panic!("The body of an if statement must be a Block"),
        }
        let end = self.op_index();
        let end_of_if = (end - start) as isize;
        self.set_op(jump_index, OP::BranchIf(end_of_if));

        StackUsage {
            popped: 0,
            pushed: 0,
        }
    }

    fn ev_return(&mut self, node_id: NodeID, proc_id: NodeID) -> StackUsage {
        let allocations = self.get_allocations(proc_id);
        self.add_op(node_id, OP::PopStack(allocations));
        self.add_op(node_id, OP::PopStackFrame);
        self.add_op(node_id, OP::PopPc);
        StackUsage::new(0, 0)
    }

    fn ev_procedure(
        &mut self,
        node_id: NodeID,
        args: &[NodeID],
        returns: &Option<String>,
        body_id: NodeID,
    ) -> StackUsage {
        let proc = self.with_scope(node_id, ContextType::StackFrame, |bc| {
            for arg in args.iter() {
                bc.add_var(*arg);
            }
            let body_node = bc.ast.get_node(body_id);
            match &body_node.body {
                NodeBody::Block(children) => {
                    let usage = bc.ev_block(body_id, children);
                    assert_eq!(StackUsage::zero(), usage);
                }
                _ => panic!("The body of a loop statement must be a Block"),
            };
        });

        let proc_offset = self.add_proc(proc);
        let value = Value::FunctionPointer(proc_offset);
        self.add_op(node_id, OP::PushImmediate(value));

        StackUsage::new(0, 1)
    }

    fn ev_variable_value(&mut self, node_id: NodeID, val_id: NodeID) -> StackUsage {
        self.add_op(node_id, OP::SLoad(self.get_variable_offset(val_id)));
        StackUsage::new(0, 1)
    }

    fn ev_assignment(&mut self, node_id: NodeID, var_id: NodeID, expr_id: NodeID) -> StackUsage {
        let expr = self.ast.get_node(expr_id);
        let usage = match &expr.body {
            NodeBody::ConstValue(value) => match value {
                NodeValue::Int(val) => self.ev_const(expr_id, Value::Int(*val)),
                _ => unimplemented!(),
            },
            _ => self.ev_node(expr_id),
        };
        assert_eq!(usage.pushed, 1);

        self.add_op(node_id, OP::SStore(self.get_variable_offset(var_id)));
        StackUsage::new(usage.popped, 0)
    }

    fn ev_declaration(&mut self, node_id: NodeID, expr: Option<NodeID>) -> StackUsage {
        if let Some(expr_id) = expr {
            self.ev_assignment(node_id, node_id, expr_id)
        } else {
            StackUsage::zero()
        }
    }

    fn ev_call(&mut self, node_id: NodeID, proc_id: NodeID, args: &[NodeID]) -> StackUsage {
        let pc_index = self.add_op(node_id, OP::Panic);
        self.add_op(node_id, OP::PushStackFrame);
        for arg in args {
            let usage = self.ev_expression(*arg);
            assert_eq!(1, usage.pushed);
            assert_eq!(0, usage.popped);
        }
        self.add_op(node_id, OP::SetStackFrame(-(args.len() as isize)));
        self.add_op(node_id, OP::Call(self.get_proc_address(proc_id)));
        let offset = self.op_index() - pc_index - 1;
        self.set_op(pc_index, OP::PushPc(offset as isize));

        StackUsage::new(0, 0)
    }

    fn ev_block(&mut self, node_id: NodeID, children: &[NodeID]) -> StackUsage {
        let allocations = self.with_context(node_id, ContextType::Block, |bc| {
            for child_id in children {
                let node = bc.ast.get_node(*child_id);
                match &node.body {
                    NodeBody::VariableDeclaration(..) | NodeBody::ConstDeclaration(..) => {
                        bc.add_var(*child_id);
                        bc.add_op(*child_id, OP::PushImmediate(Value::Unset));
                    }
                    _ => (),
                }
            }

            let mut stack_usage = StackUsage::new(0, 0);
            for child in children {
                stack_usage += bc.ev_node(*child);
                if stack_usage.popped > stack_usage.pushed {
                    panic!("More elements have been popped than pushed");
                }
            }
            assert_eq!(stack_usage.pushed, stack_usage.popped);
            bc.get_context().variables.len()
        });
        self.add_op(node_id, OP::PopStack(allocations));
        return StackUsage::new(0, 0);
    }

    fn ev_prefix_operation(
        &mut self,
        node_id: NodeID,
        op: ArithmeticOP,
        expr_id: NodeID,
    ) -> StackUsage {
        match op {
            ArithmeticOP::Add => self.ev_expression(expr_id),
            ArithmeticOP::Sub => {
                self.add_op(node_id, OP::PushImmediate(Value::Int(0)));
                let usage = self.ev_expression(expr_id);
                assert_eq!(1, usage.pushed);
                self.add_op(node_id, OP::SubI);
                StackUsage::new(usage.popped, 1)
            }
            _ => panic!("Invalid prefix operation: {:?}", op),
        }
    }

    fn ev_operation(
        &mut self,
        node_id: NodeID,
        op: ArithmeticOP,
        expr1: NodeID,
        expr2: NodeID,
    ) -> StackUsage {
        let usage1 = self.ev_expression(expr1);
        assert_eq!(usage1.pushed, 1);
        let usage2 = self.ev_expression(expr2);
        assert_eq!(usage2.pushed, 1);

        match op {
            ArithmeticOP::Add => self.add_op(node_id, OP::AddI),
            ArithmeticOP::Sub => self.add_op(node_id, OP::SubI),
            ArithmeticOP::Mul => self.add_op(node_id, OP::MulI),
            ArithmeticOP::Div => self.add_op(node_id, OP::DivI),
            ArithmeticOP::Eq => self.add_op(node_id, OP::CmpI),
        };
        StackUsage::new(usage1.popped + usage2.popped, 1)
    }

    fn ev_expression(&mut self, expr_id: NodeID) -> StackUsage {
        use crate::ast::NodeBody::*;
        use crate::ast::NodeValue::*;
        let expr = self.ast.get_node(expr_id);
        match &expr.body {
            Op(op, expr1, expr2) => self.ev_operation(expr.id, *op, *expr1, *expr2),
            ConstValue(value) => match value {
                Int(primitive) => self.ev_const(expr.id, Value::Int(*primitive)),
                _ => unimplemented!(),
            },
            VariableValue(value) => self.ev_variable_value(expr.id, *value),
            Call(proc_id, args) => self.ev_call(expr.id, *proc_id, args),
            _ => panic!("Unsupported node {:?} used as expression", expr),
        }
    }

    fn ev_const(&mut self, node_id: NodeID, value: Value) -> StackUsage {
        self.add_op(node_id, OP::PushImmediate(value.clone()));
        StackUsage::new(0, 1)
    }
}
