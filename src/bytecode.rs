use crate::ast;
use crate::ast::{Ast, NodeBody, NodeID, NodeType, NodeValue};
use crate::token::ArithmeticOP;
use std::fmt;
use std::fmt::Formatter;
use std::ops::AddAssign;

pub fn from_ast(ast: &ast::Ast) -> Bytecode {
    let root_id = ast.root();
    let mut bc = Generator::new(ast);
    let root_scope = bc.evaluate(root_id);
    bc.get_bytecode(root_scope)
}

pub struct Bytecode {
    pub code: Vec<Instruction>,
}

impl fmt::Debug for Bytecode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for (i, inst) in self.code.iter().enumerate() {
            write!(f, "{:<5}{:?}\n", i, inst)?;
        }
        write!(f, "\n")
    }
}

#[derive(Clone)]
pub struct Instruction {
    pub node_id: NodeID,
    pub op: OP,
}

impl fmt::Debug for Instruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:<50}", format!("{:?}", self.op))?;
        write!(f, "    {:?}", self.node_id)
    }
}

pub type OPOffset = isize;

#[derive(Debug, PartialEq, Clone)]
pub enum SFOffset {
    Stack {
        offset: OPOffset,
        field: Option<Vec<usize>>,
    },
    Closure {
        offset: usize,
        depth: usize,
        field: Option<Vec<usize>>,
    },
}

#[derive(Debug, Clone)]
pub enum OP {
    AddI,
    SubI,
    MulI,
    DivI,
    Eq,
    GEq,

    SLoad(SFOffset),
    SStore(SFOffset),

    Branch(OPOffset),
    BranchIf(OPOffset),
    BranchIfNot(OPOffset),
    PushPc(OPOffset),
    PopPc,

    PushImmediate(Value),
    PushToClosure,
    PopStack(usize),

    PrepareStackFrame(usize),
    PushStackFrame,
    PopStackFrame,

    Jump,

    Yield,
    Halt,
    Panic,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Unset,
    Int(isize),
    Float(f64),
    Bool(bool),
    String(String),
    ProcAddress(usize),
    RuntimeFn(usize),
    Struct(Vec<Value>),
}

#[derive(Clone, Debug, PartialEq)]
enum ContextType {
    ClosureBoundary,
    Block,
    Loop { break_inst: usize },
}

struct ContextVariable {
    node_id: NodeID,
    offset: usize,
    requires_closure: bool,
}

impl ContextVariable {
    fn new(node_id: NodeID, offset: usize, requires_closure: bool) -> Self {
        Self {
            node_id,
            offset,
            requires_closure,
        }
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

    fn local_allocations(&self) -> usize {
        let mut allocations = 0;
        for var in &self.variables {
            if !var.requires_closure {
                allocations += 1;
            }
        }
        allocations
    }

    fn closure_allocations(&self) -> usize {
        let mut allocations = 0;
        for var in &self.variables {
            if var.requires_closure {
                allocations += 1;
            }
        }
        allocations
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

struct Generator<'a> {
    procedures: Vec<Instruction>,
    ast: &'a Ast,
    scopes: Vec<Scope>,
    contexts: Vec<Context>,
}

impl<'a> Generator<'a> {
    fn new(ast: &'a Ast) -> Self {
        let placeholder = Instruction {
            node_id: ast.root(),
            op: OP::Panic,
        };
        Generator {
            ast,
            procedures: vec![placeholder.clone(), placeholder.clone(), placeholder],
            scopes: Vec::new(),
            contexts: Vec::new(),
        }
    }

    fn get_bytecode(self, mut scope: Scope) -> Bytecode {
        let mut code = self.procedures;
        let entrypoint = code.len();

        // Patch entrypoint instructions
        code[0] = Instruction {
            node_id: scope.node_id,
            op: OP::PrepareStackFrame(0),
        };
        code[1] = Instruction {
            node_id: scope.node_id,
            op: OP::PushImmediate(Value::ProcAddress(entrypoint)),
        };
        code[2] = Instruction {
            node_id: scope.node_id,
            op: OP::Jump,
        };

        code.append(&mut scope.instructions);
        code.push(Instruction {
            node_id: scope.node_id,
            op: OP::Halt,
        });

        Bytecode { code }
    }

    fn evaluate(&mut self, root_id: NodeID) -> Scope {
        let scope = self.with_scope(root_id, ContextType::Block, |bc| {
            let usage = bc.ev_node(root_id);
            assert_eq!(StackUsage::zero(), usage);
        });
        assert_eq!(0, self.scopes.len());
        scope
    }

    fn get_variable_offset(
        &self,
        _: NodeID,
        var_id: NodeID,
        field: Option<Vec<usize>>,
    ) -> SFOffset {
        let mut closure_offset = 0;
        for context in self.contexts.iter().rev() {
            if let Some(var) = context.variables.iter().find(|var| var.node_id == var_id) {
                return if self.ast.get_node(var.node_id).has_closure_references() {
                    SFOffset::Closure {
                        offset: var.offset,
                        depth: closure_offset,
                        field,
                    }
                } else {
                    SFOffset::Stack {
                        offset: var.offset as isize,
                        field,
                    }
                };
            }
            if context.tp == ContextType::ClosureBoundary {
                closure_offset += 1;
            }
        }
        panic!(
            "Failed to get variable offset of {:?}",
            self.ast.get_node(var_id)
        );
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
            allocations += context.local_allocations();
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

    fn op_index(&self) -> usize {
        self.get_scope().instructions.len()
    }

    fn add_var(&mut self, var_id: NodeID) {
        let has_closure_reference = self.ast.get_node(var_id).has_closure_references();
        let offset = if has_closure_reference {
            let mut offset = 0;
            match self.contexts.last() {
                Some(context) => {
                    offset += context.closure_allocations();
                }
                None => panic!("Context missing"),
            }
            offset
        } else {
            let mut offset = 0;
            for context in self.contexts.iter().rev() {
                offset += context.local_allocations();
                if context.tp == ContextType::ClosureBoundary {
                    break;
                }
            }
            offset
        };

        let context = self.get_context_mut();
        let var = ContextVariable::new(var_id, offset, has_closure_reference);
        context.variables.push(var);
    }

    fn add_proc(&mut self, mut scope: Scope) -> usize {
        let proc_id = self.procedures.len();
        self.procedures.append(&mut scope.instructions);
        proc_id
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
            Call(proc_id, args) => self.ev_call(node_id, *proc_id, args, true),
            VariableValue(val, field) => self.ev_variable_value(node_id, *val, field),
            ConstDeclaration(_, _, expr) => self.ev_declaration(node_id, Some(*expr)),
            TypeDeclaration(_, _, expr, _) => self.ev_declaration(node_id, Some(*expr)),
            VariableDeclaration(_, _, expr) => self.ev_declaration(node_id, *expr),
            Import(_, value) => self.ev_declaration(node_id, Some(*value)),
            VariableAssignment(var, path, value) => self.ev_assignment(node_id, *var, path, *value),
            ProcedureDeclaration(args, _, body_id) => self.ev_procedure(node_id, args, *body_id),
            Return(proc_id, ret_id) => self.ev_return(node_id, *proc_id, *ret_id),
            If(condition, body) => self.ev_if(node_id, *condition, *body),
            Loop(body) => self.ev_loop(node_id, *body),
            Break(loop_id) => self.ev_break(node_id, *loop_id),
            Comment(_) => StackUsage::zero(),
            _ => panic!("Unsupported node here {:?}", node),
        }
    }

    fn ev_break(&mut self, node_id: NodeID, loop_id: NodeID) -> StackUsage {
        let allocations = self.get_allocations(loop_id);
        if allocations > 0 {
            self.add_op(node_id, OP::PopStack(allocations));
        }
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
        self.set_op(break_inst, OP::Branch(end as isize - start as isize + 1));
        StackUsage {
            popped: 0,
            pushed: 0,
        }
    }

    fn ev_if(&mut self, node_id: NodeID, condition_id: NodeID, body_id: NodeID) -> StackUsage {
        let usage = self.ev_expression(condition_id);
        assert_eq!(usage.pushed - usage.popped, 1);
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
        self.set_op(jump_index, OP::BranchIfNot(end_of_if));

        StackUsage {
            popped: 0,
            pushed: 0,
        }
    }

    fn ev_return(
        &mut self,
        node_id: NodeID,
        proc_id: NodeID,
        ret_value: Option<NodeID>,
    ) -> StackUsage {
        if let Some(ret_id) = ret_value {
            let usage = self.ev_expression(ret_id);
            assert_eq!(usage.pushed, 1);
            assert_eq!(usage.popped, 0);
            self.add_op(
                ret_id,
                OP::SStore(SFOffset::Stack {
                    offset: -3,
                    field: None,
                }),
            );
        }
        let allocations = self.get_allocations(proc_id);
        if allocations > 0 {
            self.add_op(node_id, OP::PopStack(allocations));
        }
        self.add_op(node_id, OP::PopStackFrame);
        self.add_op(node_id, OP::PopPc);
        StackUsage::new(0, 0)
    }

    fn ev_procedure(&mut self, node_id: NodeID, args: &[NodeID], body_id: NodeID) -> StackUsage {
        let scope = self.with_scope(node_id, ContextType::ClosureBoundary, |bc| {
            for arg in args.iter() {
                bc.add_var(*arg);
            }
            let body_node = bc.ast.get_node(body_id);
            match &body_node.body {
                NodeBody::Block(children) => {
                    let usage = bc.ev_block(body_id, children);
                    assert_eq!(StackUsage::zero(), usage);
                }
                _ => panic!("The body of a procedure statement must be a Block"),
            };
        });
        let proc_index = self.add_proc(scope);
        self.add_op(node_id, OP::PushImmediate(Value::ProcAddress(proc_index)));
        StackUsage::new(0, 1)
    }

    fn ev_variable_value(
        &mut self,
        node_id: NodeID,
        val_id: NodeID,
        path: &Option<Vec<String>>,
    ) -> StackUsage {
        let index = self.get_field_index(val_id, path);

        self.add_op(
            node_id,
            OP::SLoad(self.get_variable_offset(node_id, val_id, index)),
        );
        StackUsage::new(0, 1)
    }

    fn get_field_index(&self, var_id: NodeID, path: &Option<Vec<String>>) -> Option<Vec<usize>> {
        if let Some(path) = path {
            let mut index_path = Vec::with_capacity(path.len());
            let mut tp = &self.ast.get_node(var_id).tp.as_ref().unwrap().tp;
            for path_name in path {
                match tp {
                    NodeType::Struct(fields) => {
                        let index = fields.iter().position(|(name, _)| name == path_name);
                        match index {
                            Some(index) => {
                                tp = &fields[index].1;
                                index_path.push(index)
                            }
                            None => unimplemented!(),
                        }
                    }
                    _ => unreachable!(),
                }
            }
            Some(index_path)
        } else {
            None
        }
    }

    fn ev_assignment(
        &mut self,
        node_id: NodeID,
        var_id: NodeID,
        path: &Option<Vec<String>>,
        expr_id: NodeID,
    ) -> StackUsage {
        let usage = self.ev_expression(expr_id);
        assert_eq!(usage.pushed, 1);

        let index = self.get_field_index(var_id, path);
        self.add_op(
            node_id,
            OP::SStore(self.get_variable_offset(node_id, var_id, index)),
        );
        StackUsage::new(usage.popped, 0)
    }

    fn ev_declaration(&mut self, node_id: NodeID, expr: Option<NodeID>) -> StackUsage {
        if let Some(expr_id) = expr {
            self.ev_assignment(node_id, node_id, &None, expr_id)
        } else {
            StackUsage::zero()
        }
    }

    fn ev_call(
        &mut self,
        node_id: NodeID,
        proc_var_id: NodeID,
        args: &[NodeID],
        ignore_return: bool,
    ) -> StackUsage {
        let node = self.ast.get_node(proc_var_id);
        let return_values = match &node.tp.as_ref().unwrap().tp {
            NodeType::Fn(_, ret) => match &**ret {
                NodeType::Void => 0,
                _ => 1,
            },
            NodeType::Type(..) => 1,
            _ => unreachable!(),
        };

        if return_values > 0 {
            self.add_op(node_id, OP::PushImmediate(Value::Unset));
        }

        let pc_index = self.add_op(node_id, OP::Panic);
        self.add_op(node_id, OP::PushStackFrame);
        for arg in args {
            let usage = self.ev_expression(*arg);
            assert_eq!(1, usage.pushed);
            assert_eq!(0, usage.popped);
        }
        self.add_op(node_id, OP::PrepareStackFrame(args.len()));
        self.add_op(
            node_id,
            OP::SLoad(self.get_variable_offset(node_id, proc_var_id, None)),
        );
        self.add_op(node_id, OP::Jump);
        let offset = self.op_index() - pc_index - 1;
        self.set_op(pc_index, OP::PushPc(offset as isize));

        if return_values == 0 {
            StackUsage::zero()
        } else if ignore_return {
            self.add_op(node_id, OP::PopStack(return_values));
            StackUsage::zero()
        } else {
            StackUsage::new(0, return_values)
        }
    }

    fn ev_block(&mut self, node_id: NodeID, children: &[NodeID]) -> StackUsage {
        let allocations = self.with_context(node_id, ContextType::Block, |bc| {
            for child_id in children {
                let node = bc.ast.get_node(*child_id);
                if node.has_closure_references() {
                    match &node.body {
                        NodeBody::VariableDeclaration(..)
                        | NodeBody::ConstDeclaration(..)
                        | NodeBody::TypeDeclaration(..)
                        | NodeBody::Import(..) => {
                            bc.add_var(*child_id);
                            bc.add_op(*child_id, OP::PushToClosure);
                        }
                        _ => (),
                    }
                } else {
                    match &node.body {
                        NodeBody::VariableDeclaration(..)
                        | NodeBody::ConstDeclaration(..)
                        | NodeBody::TypeDeclaration(..)
                        | NodeBody::Import(..) => {
                            bc.add_var(*child_id);
                            bc.add_op(*child_id, OP::PushImmediate(Value::Unset));
                        }
                        _ => (),
                    }
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
            bc.get_context().local_allocations()
        });
        if allocations > 0 {
            self.add_op(node_id, OP::PopStack(allocations));
        }
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
        assert_eq!(usage1.popped, 0);
        let usage2 = self.ev_expression(expr2);
        assert_eq!(usage2.pushed, 1);
        assert_eq!(usage1.popped, 0);

        match op {
            ArithmeticOP::Add => self.add_op(node_id, OP::AddI),
            ArithmeticOP::Sub => self.add_op(node_id, OP::SubI),
            ArithmeticOP::Mul => self.add_op(node_id, OP::MulI),
            ArithmeticOP::Div => self.add_op(node_id, OP::DivI),
            ArithmeticOP::Eq => self.add_op(node_id, OP::Eq),
            ArithmeticOP::GEq => self.add_op(node_id, OP::GEq),
        };
        StackUsage::new(usage1.popped + usage2.popped, 1)
    }

    fn ev_expression(&mut self, expr_id: NodeID) -> StackUsage {
        use crate::ast::NodeBody::*;
        let expr = self.ast.get_node(expr_id);
        match &expr.body {
            Op(op, expr1, expr2) => self.ev_operation(expr_id, *op, *expr1, *expr2),
            ConstValue(value) => self.ev_const(expr_id, value),
            ProcedureDeclaration(args, _, body_id) => self.ev_procedure(expr_id, args, *body_id),
            VariableValue(value, field) => self.ev_variable_value(expr_id, *value, field),
            Call(proc_id, args) => self.ev_call(expr_id, *proc_id, args, false),
            Expression(value) => self.ev_expression(*value),
            _ => panic!("Unsupported node {:?} used as expression", expr),
        }
    }

    fn default_value(&self, node_value: &NodeValue) -> Value {
        use NodeValue::*;
        match node_value {
            Int(val) => Value::Int(*val),
            Float(val) => Value::Float(*val),
            Bool(val) => Value::Bool(*val),
            String(val) => Value::String(val.clone()),
            RuntimeFn(id) => Value::RuntimeFn(*id),
            Struct(val) => Value::Struct(val.iter().map(|(_, v)| self.default_value(v)).collect()),
            Unlinked(_) => unreachable!(),
        }
    }

    fn ev_const(&mut self, node_id: NodeID, node_value: &NodeValue) -> StackUsage {
        let value = self.default_value(node_value);
        self.add_op(node_id, OP::PushImmediate(value));
        StackUsage::new(0, 1)
    }
}
