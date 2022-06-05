use crate::vm::ast::{
    ArithmeticOP, Ast, AstBranch, AstBranchID, DebugSymbols, IsLinked, IsTypesChecked,
    IsTypesInferred, IsValid, LNBTypeDeclaration, LinkedNodeBody, NBCall, NBProcedureDeclaration,
    NodeID, NodeType, NodeValue,
};
use crate::vm::token::TokenSourceInfo;
use crate::{vm, LogLevel};
use std::collections::HashMap;
use std::fmt;
use std::fmt::{Debug, Formatter};
use std::marker::PhantomData;
use std::ops::AddAssign;
use std::sync::Arc;

pub struct BytecodeGenerator<'a> {
    tokio_runtime: &'a tokio::runtime::Runtime,
    log_level: LogLevel,
}

impl<'a> BytecodeGenerator<'a> {
    pub fn new(tokio_runtime: &'a tokio::runtime::Runtime, log_level: LogLevel) -> Self {
        Self {
            tokio_runtime,
            log_level,
        }
    }

    pub fn generate(&self, asts: vm::Ast, leak_memory: bool) -> crate::Result<vm::Bytecode> {
        self.tokio_runtime.block_on(async {
            let log_level = self.log_level;
            let asts = Arc::new(asts);
            let node_id = asts.root();
            let start = vm::start_timer();
            let (scope, context) = allocate_global_variables(&asts);
            let start = vm::time("Bytecode Global allocations", start, log_level);
            let mut init_instructions = scope.instructions;

            let (tx, mut rx) = tokio::sync::mpsc::unbounded_channel();

            for ast_id in (&asts).ids() {
                let tx = tx.clone();
                let asts = asts.clone();
                let context = Some(context.clone());

                tokio::task::spawn_blocking(move || {
                    let ast = &asts.read_ast(ast_id);
                    let mut bc = Generator::new(context);
                    let (global, local) = bc.evaluate(ast);
                    let procedures = bc.procedures;
                    tx.send(Ok((ast_id, global, local, procedures, bc.procedure_len)))
                        .unwrap();
                    if false {
                        tx.send(Err(())).unwrap();
                    }
                });
            }

            let mut proc_instruction_len = vec![0; asts.len()];
            let mut global_instructions = vec![vec![]; asts.len()];
            let mut instructions = vec![vec![]; asts.len()];
            let mut proc_instructions = vec![vec![]; asts.len()];
            let mut halt_instructions = {
                let mut inst = vec![];
                let allocations = context.local_allocations();
                if allocations > 0 {
                    inst.push(Instruction::new(node_id, OPCode::PopStack(allocations)));
                }
                inst.push(Instruction::new(node_id, OPCode::Halt));
                inst
            };
            let mut proc_start_offset = init_instructions.len() + halt_instructions.len();

            drop(tx);
            while let Some(msg) = rx.recv().await {
                match msg {
                    Ok((ast_id, global, local, procedures, proc_length)) => {
                        let ast_i = ast_id.index();
                        proc_start_offset += global.len() + local.len();
                        global_instructions[ast_i] = global;
                        instructions[ast_i] = local;
                        proc_instructions[ast_i] = procedures;
                        proc_instruction_len[ast_i] = proc_length;
                    }
                    _ => unimplemented!(),
                }
            }

            let start = vm::time("Bytecode Local allocations", start, log_level);
            let mut proc_offsets = vec![0; asts.len()];
            for ast_id in asts.ids() {
                let ast_i = ast_id.index();
                proc_offsets[ast_i] = proc_start_offset;
                proc_start_offset += proc_instruction_len[ast_i];
            }

            let (tx, mut rx) = tokio::sync::mpsc::unbounded_channel();

            for (ast_i, ((mut global, mut local), mut proc)) in global_instructions
                .into_iter()
                .zip(instructions.into_iter())
                .zip(proc_instructions.into_iter())
                .enumerate()
            {
                let offset = proc_offsets[ast_i];
                let tx = tx.clone();
                tokio::task::spawn_blocking(move || {
                    for inst in global
                        .iter_mut()
                        .chain(local.iter_mut())
                        .chain(proc.iter_mut().flatten())
                    {
                        if let OPCode::PushImmediate(Value::ProcAddress(proc_index)) = &mut inst.op
                        {
                            *proc_index += offset;
                        }
                    }
                    tx.send((ast_i, global, local, proc)).unwrap();
                });
            }

            let mut global_instructions = vec![vec![]; asts.len()];
            let mut instructions = vec![vec![]; asts.len()];
            let mut proc_instructions = vec![vec![]; asts.len()];
            let n_insturctions = proc_start_offset
                + proc_instruction_len.iter().sum::<usize>()
                + halt_instructions.len();
            drop(tx);
            while let Some((ast_i, global, local, proc)) = rx.recv().await {
                global_instructions[ast_i] = global;
                instructions[ast_i] = local;
                proc_instructions[ast_i] = proc;
            }
            let start = vm::time("Bytecode patch allocations", start, log_level);

            let bytecode_task = tokio::task::spawn_blocking(move || {
                let mut code = Vec::with_capacity(n_insturctions);
                code.append(&mut init_instructions);
                for mut i in global_instructions {
                    code.append(&mut i);
                }
                for mut i in instructions {
                    code.append(&mut i);
                }
                code.append(&mut halt_instructions);
                for proc in proc_instructions {
                    for mut p in proc {
                        code.append(&mut p);
                    }
                }
                vm::time("Bytecode output", start, log_level);
                code
            });

            let symbol_task = {
                let asts = asts.clone();
                tokio::task::spawn(async move {
                    let tasks = asts.ids().map(|ast_id| {
                        let asts = asts.clone();
                        tokio::task::spawn_blocking(move || {
                            let ast = asts.read_ast(ast_id);
                            let default_token = TokenSourceInfo::default();
                            ast.nodes()
                                .iter()
                                .enumerate()
                                .map(|(i, node)| {
                                    assert_eq!(i, node.id().index());
                                    let start = node
                                        .tokens
                                        .first()
                                        .map(|t| t.source)
                                        .unwrap_or(default_token);
                                    let end = node
                                        .tokens
                                        .last()
                                        .map(|t| t.source)
                                        .unwrap_or(default_token);
                                    (start, end)
                                })
                                .collect::<Vec<(TokenSourceInfo, TokenSourceInfo)>>()
                        })
                    });
                    let file_info = futures::future::try_join_all(tasks).await.unwrap();
                    let files = asts
                        .iter()
                        .enumerate()
                        .map(|(i, ast)| {
                            assert_eq!(i, ast.id().index());
                            ast.path.clone()
                        })
                        .collect();

                    vm::time("Bytecode debug symbols", start, log_level);
                    Some(DebugSymbols { file_info, files })
                })
            };

            let (code, debug_symbols) = tokio::try_join!(bytecode_task, symbol_task).unwrap();
            let start = vm::start_timer();
            let asts = Arc::try_unwrap(asts).expect("Single instance of ast in bytecode");
            if leak_memory {
                Box::leak(Box::new(asts));
            } else {
                drop(asts);
            }
            vm::time("De-allocating ast", start, log_level);

            if self.log_level >= LogLevel::LogEval {
                println!(
                    "Instructions:\n{}",
                    code.iter()
                        .enumerate()
                        .map(|(i, c)| format!("{:>4}: {:?}", i, c))
                        .collect::<Vec<String>>()
                        .join("\n")
                );
            }

            Ok(Bytecode {
                code,
                debug_symbols,
            })
        })
    }
}

fn allocate_global_variables<T>(asts: &Ast<T>) -> (Scope, Context)
where
    T: IsLinked + IsTypesInferred + IsTypesChecked,
{
    let mut bc = Generator::new(None);
    let root_id = asts.root();

    let (scope, context) = bc.with_scope(root_id, ContextType::Block, |bc| {
        for ast in asts.iter() {
            let root_id = ast.root();
            let node = asts.get_node(root_id);
            if let LinkedNodeBody::Block { static_body, .. } = &*node.body {
                bc.ev_block_allocate_variables(&ast, &[static_body]);
            } else {
                unreachable!()
            }
        }
    });
    (scope, context)
}

pub struct Bytecode {
    pub code: Vec<Instruction>,
    pub debug_symbols: Option<DebugSymbols>,
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
    pub op: OPCode,
}

impl Instruction {
    pub fn new(node_id: NodeID, op: OPCode) -> Self {
        Self { node_id, op }
    }
}

impl fmt::Debug for Instruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:<30}", format!("{:?}", self.op))?;
        write!(f, "    {:?}", self.node_id)
    }
}

pub type OPOffset = isize;

#[derive(PartialEq, Clone)]
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

impl Debug for SFOffset {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            SFOffset::Stack {
                offset,
                field: Option::None,
            } => write!(f, "{}", offset),
            SFOffset::Stack {
                offset,
                field: Option::Some(field),
            } => write!(f, "{}.{:?}", offset, field),
            SFOffset::Closure {
                offset,
                depth,
                field: Option::None,
            } => write!(f, "closure({}) {}", depth, offset),
            SFOffset::Closure {
                offset,
                depth,
                field: Option::Some(field),
            } => write!(f, "closure({}) {}.{:?}", depth, offset, field),
        }
    }
}

#[derive(Debug, Clone)]
#[non_exhaustive]
pub enum OPCode {
    AddI,
    SubI,
    MulI,
    DivI,
    Eq,
    GEq,
    LEq,

    SLoad(SFOffset),
    SStore(SFOffset),

    Branch(OPOffset),
    BranchIfNot(OPOffset),
    PushPc(OPOffset),
    PopPc,

    PushImmediate(Value),
    PushToClosure(Value),
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
#[non_exhaustive]
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

#[derive(Clone)]
struct Context {
    node_id: NodeID,
    closure_variables: HashMap<NodeID, usize>,
    local_variables: HashMap<NodeID, usize>,
    tp: ContextType,
}

impl Context {
    fn new(node_id: NodeID, tp: ContextType) -> Self {
        let local_variables = HashMap::new();
        let closure_variables = HashMap::new();
        Self {
            node_id,
            tp,
            local_variables,
            closure_variables,
        }
    }

    fn local_allocations(&self) -> usize {
        self.allocations(false)
    }

    fn allocations(&self, has_closure_reference: bool) -> usize {
        if has_closure_reference {
            self.closure_variables.len()
        } else {
            self.local_variables.len()
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

struct Generator<T>
where
    T: IsValid,
{
    procedures: Vec<Vec<Instruction>>,
    procedure_len: usize,
    scopes: Vec<Scope>,
    contexts: Vec<Context>,
    global_context: Option<Context>,
    dummy_offset: usize,
    _tp: PhantomData<T>,
}

impl<T> Generator<T>
where
    T: IsLinked + IsTypesInferred + IsTypesChecked,
{
    fn new(global_context: Option<Context>) -> Self {
        Generator {
            procedure_len: 0,
            procedures: Vec::new(),
            scopes: Vec::new(),
            contexts: Vec::new(),
            dummy_offset: 0,
            global_context,
            _tp: PhantomData::default(),
        }
    }

    fn evaluate(&mut self, ast: &AstBranch<T>) -> (Vec<Instruction>, Vec<Instruction>) {
        self.dummy_offset = ast.nodes().len();
        let root_id = ast.root();
        let (global_scope, new_context) = self.with_scope(root_id, ContextType::Block, |bc| {
            let node = ast.get_node(root_id);
            if let LinkedNodeBody::Block { static_body, .. } = &*node.body {
                bc.ev_block_inner(ast, &[static_body]);
            } else {
                unreachable!()
            }
        });
        assert!(new_context.closure_variables.is_empty());
        assert!(new_context.local_variables.is_empty());

        let (ast_scope, _) = self.with_scope(root_id, ContextType::Block, |bc| {
            let node = ast.get_node(root_id);
            if let LinkedNodeBody::Block {
                import_body,
                dynamic_body,
                ..
            } = &*node.body
            {
                bc.ev_block_allocate_variables(ast, &[import_body, dynamic_body]);
                let allocations = bc.ev_block_inner(ast, &[import_body, dynamic_body]);
                if allocations > 0 {
                    bc.add_op(root_id, OPCode::PopStack(allocations));
                }
            } else {
                unreachable!()
            };
        });
        assert_eq!(0, self.scopes.len());

        (global_scope.instructions, ast_scope.instructions)
    }

    fn context_rev(&self) -> impl Iterator<Item = &Context> {
        self.contexts.iter().rev().chain(self.global_context.iter())
    }

    fn get_variable_offset(&self, var_id: NodeID, field: Option<Vec<usize>>) -> SFOffset {
        let mut closure_offset = 0;
        for context in self.context_rev() {
            if let Some(var) = context.local_variables.get(&var_id) {
                return SFOffset::Stack {
                    offset: (*var) as isize,
                    field,
                };
            } else if let Some(var) = context.closure_variables.get(&var_id) {
                return SFOffset::Closure {
                    offset: *var,
                    depth: closure_offset,
                    field,
                };
            }
            if context.tp == ContextType::ClosureBoundary {
                closure_offset += 1;
            }
        }
        panic!("Failed to get variable offset of {:?}", var_id);
    }

    fn get_loop_break_offset(&self, loop_id: NodeID) -> OPOffset {
        for context in self.context_rev() {
            if context.node_id == loop_id {
                match context.tp {
                    ContextType::Loop { break_inst } => {
                        return break_inst as isize - self.op_index() as isize - 1;
                    }
                    _ => unreachable!(),
                }
            }
        }
        panic!("Failed to find loop searching for {:?}", loop_id);
    }

    fn get_allocations(&self, node_id: NodeID) -> usize {
        let mut allocations = 0;
        for context in self.context_rev() {
            allocations += context.local_allocations();
            if context.node_id == node_id {
                return allocations;
            }
        }
        panic!(
            "Failed to find node when counting allocations: {:?}",
            node_id
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

    fn new_var_offset(&self, has_closure_reference: bool) -> usize {
        let mut offset = 0;
        for context in self.context_rev() {
            offset += context.allocations(has_closure_reference);
            if context.tp == ContextType::ClosureBoundary {
                break;
            }
        }
        offset
    }

    fn add_var(&mut self, ast: &AstBranch<T>, var_id: NodeID) {
        let has_closure_reference = ast.get_node(var_id).has_closure_references();
        let offset = self.new_var_offset(has_closure_reference);
        let context = self.get_context_mut();
        if has_closure_reference {
            context.closure_variables.insert(var_id, offset);
        } else {
            context.local_variables.insert(var_id, offset);
        }
    }

    fn add_dummy_var(&mut self, has_closure_reference: bool) {
        let offset = self.new_var_offset(has_closure_reference);
        let node_id = NodeID::new(self.dummy_offset, AstBranchID::new(usize::MAX));
        self.dummy_offset += 1;

        let context = self.get_context_mut();
        if has_closure_reference {
            context.closure_variables.insert(node_id, offset);
        } else {
            context.local_variables.insert(node_id, offset);
        }
    }

    fn add_proc(&mut self, scope: Scope) -> usize {
        let proc_id = self.procedure_len;
        self.procedure_len += scope.instructions.len();
        self.procedures.push(scope.instructions);
        proc_id
    }

    fn add_op(&mut self, node_id: NodeID, op: OPCode) {
        let instruction = Instruction { node_id, op };
        self.get_scope_mut().instructions.push(instruction);
    }

    fn set_op(&mut self, index: usize, op: OPCode) {
        self.get_scope_mut().instructions[index].op = op
    }

    fn with_context<F, K>(&mut self, node_id: NodeID, tp: ContextType, func: F) -> (Context, K)
    where
        F: FnOnce(&mut Self) -> K,
    {
        self.contexts.push(Context::new(node_id, tp));

        let result = func(self);

        let context = self.contexts.pop().unwrap();
        assert_eq!(context.node_id, node_id);

        (context, result)
    }

    fn with_scope<F>(&mut self, node_id: NodeID, tp: ContextType, func: F) -> (Scope, Context)
    where
        F: FnOnce(&mut Self),
    {
        self.scopes.push(Scope::new(node_id));

        let (context, _) = self.with_context(node_id, tp, func);

        let scope = self.scopes.pop().unwrap();
        assert_eq!(scope.node_id, node_id);
        (scope, context)
    }

    fn ev_node(&mut self, ast: &AstBranch<T>, node_id: NodeID) -> StackUsage {
        use crate::vm::ast::LinkedNodeBody::*;
        let node = ast.get_node(node_id);
        if node.is_dead() {
            return StackUsage::zero();
        }

        match &*node.body {
            Op { op, lhs, rhs } => self.ev_operation(ast, node_id, *op, *lhs, *rhs),
            PrefixOp { op, rhs } => self.ev_prefix_operation(ast, node_id, *op, *rhs),
            Block {
                static_body,
                import_body,
                dynamic_body,
            } => self.ev_block(ast, node_id, &[static_body, import_body, dynamic_body]),
            Call(NBCall { func, args }) => self.ev_call(ast, node_id, *func, args, true),
            VariableValue { variable, path } => {
                self.ev_variable_value(ast, node_id, *variable, path)
            }
            TypeDeclaration(LNBTypeDeclaration { constructor, .. }) => {
                self.ev_declaration(ast, node_id, Some(**constructor))
            }
            ConstDeclaration { expr, .. }
            | StaticDeclaration { expr, .. }
            | Import { expr, .. } => self.ev_declaration(ast, node_id, Some(*expr)),
            VariableDeclaration { expr, .. } => self.ev_declaration(ast, node_id, *expr),
            VariableAssignment {
                variable,
                path,
                expr,
            } => self.ev_assignment(ast, node_id, *variable, path, *expr),
            ProcedureDeclaration(NBProcedureDeclaration { args, body, .. }) => {
                self.ev_procedure(ast, node_id, args, *body)
            }
            Return { func, expr, .. } => self.ev_return(ast, node_id, *func, *expr),
            If { condition, body } => self.ev_if(ast, node_id, *condition, *body),
            Loop { body } => self.ev_loop(ast, node_id, *body),
            Break { r#loop } => self.ev_break(node_id, *r#loop),
            Comment(_) => StackUsage::zero(),
            _ => panic!("Unsupported node here {:?}", node_id),
        }
    }

    fn ev_break(&mut self, node_id: NodeID, loop_id: NodeID) -> StackUsage {
        let allocations = self.get_allocations(loop_id);
        if allocations > 0 {
            self.add_op(node_id, OPCode::PopStack(allocations));
        }
        let offset = self.get_loop_break_offset(loop_id);
        self.add_op(node_id, OPCode::Branch(offset));
        StackUsage::new(0, 0)
    }

    fn ev_loop(&mut self, ast: &AstBranch<T>, node_id: NodeID, body_id: NodeID) -> StackUsage {
        self.add_op(node_id, OPCode::Branch(1));
        let break_inst = self.op_index();
        self.add_op(node_id, OPCode::Panic);
        let start = self.op_index();
        self.with_context(node_id, ContextType::Loop { break_inst }, |bc| {
            let body_node = ast.get_node(body_id);
            match &*body_node.body {
                LinkedNodeBody::Block {
                    static_body,
                    import_body,
                    dynamic_body,
                } => {
                    let usage =
                        bc.ev_block(ast, body_id, &[static_body, import_body, dynamic_body]);
                    assert_eq!(usage.pushed, usage.popped);
                }
                _ => panic!("The body of a loop statement must be a scope"),
            };
        });
        let end = self.op_index();
        self.add_op(node_id, OPCode::Branch(start as isize - end as isize - 1));
        self.set_op(
            break_inst,
            OPCode::Branch(end as isize - start as isize + 1),
        );
        StackUsage {
            popped: 0,
            pushed: 0,
        }
    }

    fn ev_if(
        &mut self,
        ast: &AstBranch<T>,
        node_id: NodeID,
        condition_id: NodeID,
        body_id: NodeID,
    ) -> StackUsage {
        let usage = self.ev_expression(ast, condition_id);
        assert_eq!(usage.pushed - usage.popped, 1);
        let jump_index = self.op_index();
        self.add_op(node_id, OPCode::Panic);

        let start = self.op_index();
        let body_node = ast.get_node(body_id);
        match &*body_node.body {
            LinkedNodeBody::Block {
                static_body,
                import_body,
                dynamic_body,
            } => {
                let usage = self.ev_block(ast, body_id, &[static_body, import_body, dynamic_body]);
                assert_eq!(usage.pushed, usage.popped);
            }
            _ => panic!("The body of an if statement must be a Block"),
        }
        let end = self.op_index();
        let end_of_if = (end - start) as isize;
        self.set_op(jump_index, OPCode::BranchIfNot(end_of_if));

        StackUsage {
            popped: 0,
            pushed: 0,
        }
    }

    fn ev_return(
        &mut self,
        ast: &AstBranch<T>,
        node_id: NodeID,
        proc_id: NodeID,
        ret_value: Option<NodeID>,
    ) -> StackUsage {
        if let Some(ret_id) = ret_value {
            let usage = self.ev_expression(ast, ret_id);
            assert_eq!(usage.pushed, 1);
            assert_eq!(usage.popped, 0);
            self.add_op(
                ret_id,
                OPCode::SStore(SFOffset::Stack {
                    offset: -3,
                    field: None,
                }),
            );
        }
        let allocations = self.get_allocations(proc_id);
        if allocations > 0 {
            self.add_op(node_id, OPCode::PopStack(allocations));
        }
        self.add_op(node_id, OPCode::PopStackFrame);
        self.add_op(node_id, OPCode::PopPc);
        StackUsage::new(0, 0)
    }

    fn ev_procedure(
        &mut self,
        ast: &AstBranch<T>,
        node_id: NodeID,
        args: &[NodeID],
        body_id: NodeID,
    ) -> StackUsage {
        let (scope, _) = self.with_scope(node_id, ContextType::ClosureBoundary, |bc| {
            for (i, arg) in args.iter().enumerate() {
                if ast.get_node(*arg).has_closure_references() {
                    // All variables are by default passed to function on the stack.
                    // If the argument is referenced somewhere inside an inner function,
                    // it has to be move to a closure instead.

                    // First create a dummy variable for the original argument, this ensures that
                    // deallocating works as expected. Then copy the variable to the top of the stack.
                    bc.add_dummy_var(false);
                    bc.add_op(
                        *arg,
                        OPCode::SLoad(SFOffset::Stack {
                            offset: i as isize,
                            field: None,
                        }),
                    );

                    // We can then create the true variable which we will later look up and copy the
                    // data from the argument, now on top of the stack, to the closure.
                    let offset = bc.new_var_offset(true);
                    bc.add_op(*arg, OPCode::PushToClosure(Value::Unset));
                    bc.add_op(
                        *arg,
                        OPCode::SStore(SFOffset::Closure {
                            offset,
                            depth: 0,
                            field: None,
                        }),
                    );
                    bc.add_var(ast, *arg);
                } else {
                    bc.add_var(ast, *arg);
                }
            }
            let body_node = ast.get_node(body_id);
            match &*body_node.body {
                LinkedNodeBody::Block {
                    static_body,
                    import_body,
                    dynamic_body,
                } => {
                    let usage =
                        bc.ev_block(ast, body_id, &[static_body, import_body, dynamic_body]);
                    assert_eq!(StackUsage::zero(), usage);
                }
                _ => panic!("The body of a procedure statement must be a Block"),
            };
        });
        let proc_index = self.add_proc(scope);
        self.add_op(
            node_id,
            OPCode::PushImmediate(Value::ProcAddress(proc_index)),
        );
        StackUsage::new(0, 1)
    }

    fn ev_variable_value(
        &mut self,
        ast: &AstBranch<T>,
        node_id: NodeID,
        val_id: NodeID,
        path: &Option<Vec<String>>,
    ) -> StackUsage {
        let index = self.get_field_index(ast, val_id, path);
        let offset = self.get_variable_offset(val_id, index);

        self.add_op(node_id, OPCode::SLoad(offset));
        StackUsage::new(0, 1)
    }

    fn get_field_index(
        &self,
        ast: &AstBranch<T>,
        var_id: NodeID,
        path: &Option<Vec<String>>,
    ) -> Option<Vec<usize>> {
        if let Some(path) = path {
            let mut index_path = Vec::with_capacity(path.len());
            let node = ast.get_node(var_id);
            let mut tp = node.tp();
            for path_name in path {
                let fields = if let NodeType::Struct { fields } = &tp {
                    fields
                } else if let NodeType::Type { content, .. } = &tp {
                    if let NodeType::Struct { fields } = &**content {
                        fields
                    } else {
                        unimplemented!("{}", tp)
                    }
                } else {
                    unimplemented!("{}", tp)
                };
                let index = fields.iter().position(|(name, _)| name == path_name);
                match index {
                    Some(index) => {
                        tp = &fields[index].1;
                        index_path.push(index)
                    }
                    None => unimplemented!(),
                }
            }
            Some(index_path)
        } else {
            None
        }
    }

    fn ev_assignment(
        &mut self,
        ast: &AstBranch<T>,
        node_id: NodeID,
        var_id: NodeID,
        path: &Option<Vec<String>>,
        expr_id: NodeID,
    ) -> StackUsage {
        let usage = self.ev_expression(ast, expr_id);
        assert_eq!(usage.pushed, 1);

        let index = self.get_field_index(ast, var_id, path);
        self.add_op(
            node_id,
            OPCode::SStore(self.get_variable_offset(var_id, index)),
        );
        StackUsage::new(usage.popped, 0)
    }

    fn ev_declaration(
        &mut self,
        ast: &AstBranch<T>,
        node_id: NodeID,
        expr: Option<NodeID>,
    ) -> StackUsage {
        if let Some(expr_id) = expr {
            self.ev_assignment(ast, node_id, node_id, &None, expr_id)
        } else {
            StackUsage::zero()
        }
    }

    fn ev_call(
        &mut self,
        ast: &AstBranch<T>,
        node_id: NodeID,
        proc_var_id: NodeID,
        args: &[NodeID],
        ignore_return: bool,
    ) -> StackUsage {
        let node = ast.get_node(proc_var_id);
        let return_values = match node.tp() {
            NodeType::Fn { returns, .. } => match &**returns {
                NodeType::Void => 0,
                _ => 1,
            },
            NodeType::NewType { .. } => 1,
            _ => unreachable!(),
        };

        if return_values > 0 {
            self.add_op(node_id, OPCode::PushImmediate(Value::Unset));
        }

        let pc_index = self.op_index();
        self.add_op(node_id, OPCode::Panic);
        self.add_op(node_id, OPCode::PushStackFrame);
        for arg in args {
            let usage = self.ev_expression(ast, *arg);
            assert_eq!(1, usage.pushed);
            assert_eq!(0, usage.popped);
        }
        self.add_op(node_id, OPCode::PrepareStackFrame(args.len()));
        let offset = self.get_variable_offset(proc_var_id, None);
        self.add_op(node_id, OPCode::SLoad(offset));
        self.add_op(node_id, OPCode::Jump);
        let offset = self.op_index() - pc_index - 1;
        self.set_op(pc_index, OPCode::PushPc(offset as isize));

        if return_values == 0 {
            StackUsage::zero()
        } else if ignore_return {
            self.add_op(node_id, OPCode::PopStack(return_values));
            StackUsage::zero()
        } else {
            StackUsage::new(0, return_values)
        }
    }

    fn ev_block_allocate_variables(&mut self, ast: &AstBranch<T>, body: &[&[NodeID]]) {
        for body_part in body.iter() {
            for child_id in (*body_part).iter() {
                let child_id = *child_id;
                let node = ast.get_node(child_id);
                if node.is_dead() {
                    continue;
                }
                if node.has_closure_references() {
                    match &*node.body {
                        LinkedNodeBody::VariableDeclaration { .. }
                        | LinkedNodeBody::ConstDeclaration { .. }
                        | LinkedNodeBody::TypeDeclaration(LNBTypeDeclaration { .. })
                        | LinkedNodeBody::StaticDeclaration { .. }
                        | LinkedNodeBody::Import { .. } => {
                            self.add_var(ast, child_id);
                            self.add_op(child_id, OPCode::PushToClosure(Value::Unset));
                        }
                        _ => (),
                    }
                } else {
                    match &*node.body {
                        LinkedNodeBody::VariableDeclaration { .. }
                        | LinkedNodeBody::ConstDeclaration { .. }
                        | LinkedNodeBody::TypeDeclaration(LNBTypeDeclaration { .. })
                        | LinkedNodeBody::StaticDeclaration { .. }
                        | LinkedNodeBody::Import { .. } => {
                            self.add_var(ast, child_id);
                            self.add_op(child_id, OPCode::PushImmediate(Value::Unset));
                        }
                        _ => (),
                    }
                }
            }
        }
    }

    fn ev_block_inner(&mut self, ast: &AstBranch<T>, body: &[&[NodeID]]) -> usize {
        let mut stack_usage = StackUsage::new(0, 0);
        for body_part in body.iter() {
            for child_id in body_part.iter() {
                let child_id = *child_id;
                stack_usage += self.ev_node(ast, child_id);
                if stack_usage.popped > stack_usage.pushed {
                    panic!("More elements have been popped than pushed");
                }
            }
        }
        assert_eq!(stack_usage.pushed, stack_usage.popped);
        self.get_context().local_allocations()
    }

    fn ev_block(&mut self, ast: &AstBranch<T>, node_id: NodeID, body: &[&[NodeID]]) -> StackUsage {
        let (_, allocations) = self.with_context(node_id, ContextType::Block, |bc| {
            bc.ev_block_allocate_variables(ast, body);
            bc.ev_block_inner(ast, body)
        });
        if allocations > 0 {
            self.add_op(node_id, OPCode::PopStack(allocations));
        }
        return StackUsage::new(0, 0);
    }

    fn ev_prefix_operation(
        &mut self,
        ast: &AstBranch<T>,
        node_id: NodeID,
        op: ArithmeticOP,
        expr_id: NodeID,
    ) -> StackUsage {
        match op {
            ArithmeticOP::Add => self.ev_expression(ast, expr_id),
            ArithmeticOP::Sub => {
                self.add_op(node_id, OPCode::PushImmediate(Value::Int(0)));
                let usage = self.ev_expression(ast, expr_id);
                assert_eq!(1, usage.pushed);
                assert_eq!(0, usage.popped);
                self.add_op(node_id, OPCode::SubI);
                StackUsage::new(usage.popped, 1)
            }
            _ => panic!("Invalid prefix operation: {}", op),
        }
    }

    fn ev_operation(
        &mut self,
        ast: &AstBranch<T>,
        node_id: NodeID,
        op: ArithmeticOP,
        expr1: NodeID,
        expr2: NodeID,
    ) -> StackUsage {
        let usage1 = self.ev_expression(ast, expr1);
        assert_eq!(usage1.pushed, 1);
        assert_eq!(usage1.popped, 0);
        let usage2 = self.ev_expression(ast, expr2);
        assert_eq!(usage2.pushed, 1);
        assert_eq!(usage1.popped, 0);

        match op {
            ArithmeticOP::Add => self.add_op(node_id, OPCode::AddI),
            ArithmeticOP::Sub => self.add_op(node_id, OPCode::SubI),
            ArithmeticOP::Mul => self.add_op(node_id, OPCode::MulI),
            ArithmeticOP::Div => self.add_op(node_id, OPCode::DivI),
            ArithmeticOP::Eq => self.add_op(node_id, OPCode::Eq),
            ArithmeticOP::GEq => self.add_op(node_id, OPCode::GEq),
            ArithmeticOP::LEq => self.add_op(node_id, OPCode::LEq),
        };
        StackUsage::new(usage1.popped + usage2.popped, 1)
    }

    fn ev_expression(&mut self, ast: &AstBranch<T>, expr_id: NodeID) -> StackUsage {
        use crate::vm::ast::LinkedNodeBody::*;
        let expr = ast.get_node(expr_id);
        match &*expr.body {
            Op { op, lhs, rhs } => self.ev_operation(ast, expr_id, *op, *lhs, *rhs),
            PrefixOp { op, rhs } => self.ev_prefix_operation(ast, expr_id, *op, *rhs),
            ConstValue { value, .. } => self.ev_const(expr_id, &*value),
            ProcedureDeclaration(NBProcedureDeclaration { args, body, .. }) => {
                self.ev_procedure(ast, expr_id, args, *body)
            }
            VariableValue { variable, path } => {
                self.ev_variable_value(ast, expr_id, *variable, path)
            }
            Reference { node_id } => self.ev_variable_value(ast, expr_id, *node_id, &None),
            Call(NBCall { func, args }) => self.ev_call(ast, expr_id, *func, args, false),
            Expression(value) => self.ev_expression(ast, *value),
            _ => panic!("Unsupported node {:?} used as expression", expr_id),
        }
    }

    fn default_value(&self, node_value: &NodeValue<T>) -> Value {
        use NodeValue::*;
        match node_value {
            Int(val) => Value::Int(*val),
            Float(val) => Value::Float(*val),
            Bool(val) => Value::Bool(*val),
            String(val) => Value::String(val.clone()),
            RuntimeFn(id) => Value::RuntimeFn(*id),
            Struct(val) => {
                Value::Struct(val.iter().map(|(_, v)| self.default_value(&*v)).collect())
            }
        }
    }

    fn ev_const(&mut self, node_id: NodeID, node_value: &NodeValue<T>) -> StackUsage {
        let value = self.default_value(node_value);
        self.add_op(node_id, OPCode::PushImmediate(value));
        StackUsage::new(0, 1)
    }
}
