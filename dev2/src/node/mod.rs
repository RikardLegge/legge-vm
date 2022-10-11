mod expression;
mod statement;
mod variable;

pub use crate::node::variable::*;

use crate::linker::{Linker, LinkerContext};
pub use crate::node::expression::{
    BlockStorage, ExpressionChainStorage, FunctionCallStorage, FunctionDeclarationStorage,
    IfStorage, LoopStorage, OperationStorage, ValueStorage, VariableValueStorage,
};
pub use crate::node::statement::{
    BreakStorage, EvaluateExpressionStorage, ReturnStorage, StaticAssignmentStorage,
    TypeDeclarationStorage, VariableAssignmentStorage, VariableDeclarationStorage,
};
use crate::types::{NodeType, NodeUsage, Types};
use crate::{ast, build_ast};
use std::borrow::Cow;

pub type Ast = ast::Ast<Any>;
pub type NodeID<T = Any> = ast::NodeID<T>;
pub type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Debug)]
pub enum Error {
    EOF,
    InternalError,
    VariableNotFound(String),
    ExpectedEndStatement,
    UnexpectedToken,
    TypeNotInferred(NodeID),
    TypeMissmatch(NodeID, NodeID),
    UnlinkedNode(NodeID),
    AstError(Ast, Box<Error>),
}

// Define Ast structure and generate conversion impls
build_ast! {
    Storage = Storage,
    Any [
        Variable(VariableStorage),
        Expression [
            If(IfStorage),
            Block(BlockStorage),
            Value(ValueStorage),
            Loop(LoopStorage),
            Operation(OperationStorage),
            ExpressionChain(ExpressionChainStorage),
            FunctionCall(FunctionCallStorage),
            FunctionDeclaration(FunctionDeclarationStorage),
            VariableValue(VariableValueStorage),
        ],
        Statement [
            VariableDeclaration(VariableDeclarationStorage),
            TypeDeclaration(TypeDeclarationStorage),
            Return(ReturnStorage),
            Break(BreakStorage),
            EvaluateExpression(EvaluateExpressionStorage),
            VariableAssignment(VariableAssignmentStorage),
            StaticAssignment(StaticAssignmentStorage),
        ],
    ]
}

// This is how the node data is stored internally.
// Should be generated by macro
#[derive(Debug)]
pub enum Storage {
    Any(()),

    Expression(()),
    ExpressionChain(ExpressionChainStorage),
    Block(BlockStorage),
    Loop(LoopStorage),
    If(IfStorage),
    Operation(OperationStorage),
    FunctionCall(FunctionCallStorage),
    FunctionDeclaration(FunctionDeclarationStorage),
    VariableValue(VariableValueStorage),
    Value(ValueStorage),

    Statement(()),
    VariableDeclaration(VariableDeclarationStorage),
    TypeDeclaration(TypeDeclarationStorage),
    Return(ReturnStorage),
    Break(BreakStorage),
    EvaluateExpression(EvaluateExpressionStorage),
    VariableAssignment(VariableAssignmentStorage),
    StaticAssignment(StaticAssignmentStorage),

    Variable(VariableStorage),
}

#[macro_export]
macro_rules! reified {
    ($node:ident $($rest:tt)*) => {
        use $crate::node::Storage;
        // Should be generated by macro
        $crate::reified! {@matches $node
            Any |
            Expression |
                Block | FunctionDeclaration | Loop | VariableValue |
                Value | If | FunctionCall | ExpressionChain | Operation |
            Statement |
                VariableDeclaration | TypeDeclaration |
                Return | Break | EvaluateExpression |
                VariableAssignment | StaticAssignment |
            Variable
                => { $node $($rest)* }
        }
    };
    (@matches $node:ident $($variant:ident)|* $(|)? => $body:tt ) => {
        match $node.storage() {
            $(Storage::$variant(_) => {
                let node_ref = $node.get_ref();
                // Safety: Only affects the marker types on AstNodeRef. The marker
                // traits are never trusted and real conversion checks are always
                // executed when extracting data.
                #[allow(clippy::useless_transmute)]
                let node: $crate::ast::AstNodeRef<$crate::node::$variant> = unsafe { std::mem::transmute(node_ref) };
                $crate::reified! { @body node $body }
            }),*
        }
    };
    (@body $node:ident {$ignore:ident$($rest:tt)*} ) => {
            $node$($rest)*
    };
}

impl Types for ast::AstNodeRef<Any> {
    fn get_type<'this, 'ast>(
        &'this self,
        ast: &'ast Ast,
        usage: NodeUsage,
    ) -> Result<Cow<'ast, NodeType>> {
        let node = ast.get(self.id);
        reified! {node.get_type(ast, usage)}
    }
}

// impl Linker for ast::AstNodeRef<Any> {
//     fn link(&self, ast: &mut Ast, context: LinkerContext) -> Result<()> {
//         let node = ast.get(self.id);
//         reified! {node.link(ast, context)}
//     }
// }
