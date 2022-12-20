mod expression;
mod statement;
mod variable;

pub use crate::node::variable::*;

use crate::ast::NodeBody;
use crate::children::Children;
use crate::linker::{LinkContext, Linker};
pub use crate::node::expression::{
    BlockStorage, ExpressionChainStorage, FunctionCallStorage, FunctionDeclarationStorage,
    IfStorage, LoopStorage, OperationStorage, ValueStorage, VariableValueStorage,
};
pub use crate::node::statement::{
    BreakStorage, EvaluateExpressionStorage, ReturnStorage, StaticAssignmentStorage,
    TypeDeclarationStorage, VariableAssignmentStorage, VariableDeclarationStorage,
};
use crate::types::{NodeType, NodeUsage, Types};
use crate::{ast, build_ast, children};
use std::borrow::Cow;
use std::fmt::{Debug, Formatter};

pub type Ast = ast::Ast<Any>;
pub type NodeID<T = Any> = ast::NodeID<T>;
pub type Result<T, E = Error> = std::result::Result<T, E>;

impl Debug for Ast {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some(root) = self.root() {
            writeln!(f, "Ast [")?;
            fmt_debug_node(self, f, 0, root)?;
            writeln!(f, "\n]")
        } else {
            write!(f, "Empty")
        }
    }
}

fn fmt_debug_node(
    ast: &Ast,
    f: &mut Formatter<'_>,
    level: usize,
    node_id: NodeID<Any>,
) -> std::fmt::Result {
    if level > 100 {
        write!(f, "Nesting level too deep!")?;
        return Ok(());
    }
    let node = ast.get(node_id);
    let mut children = node.children(ast).peekable();

    let prefix = "";
    write!(f, "{}", prefix)?;

    let pad_len = 2 + level * 2;
    let pad_start = " ".repeat(std::cmp::max(0, pad_len as isize - prefix.len() as isize) as usize);
    let pad_end = " ".repeat(pad_len);
    write!(f, "{}", pad_start)?;

    write!(f, " {:?}", node.id)?;

    let value_tp = node.get_type(ast, NodeUsage::Value);
    let type_tp = node.get_type(ast, NodeUsage::Type);
    match (&value_tp, &type_tp) {
        (Ok(value_tp), Ok(type_tp)) if value_tp == type_tp => write!(f, " : {:?}", value_tp)?,
        _ => {
            if let Ok(tp) = value_tp {
                write!(f, " :value {:?}", tp)?;
            }
            if let Ok(tp) = type_tp {
                write!(f, " :type {:?}", tp)?;
            }
        }
    }
    write!(f, " = {:?}", node.storage())?;

    if children.peek().is_some() {
        writeln!(f, " [")?;
        for child in children {
            fmt_debug_node(ast, f, level + 1, child.into())?;
            writeln!(f)?;
        }
        write!(f, " ")?;
        write!(f, "{}]", pad_end)?;
    }
    Ok(())
}

#[derive(Debug)]
pub enum Error {
    EOF,
    AstError(ast::Error<Any>),
    InternalError(String),
    VariableNotFound(String, LinkContext),
    ExpectedEndStatement,
    ExpectedParent(NodeID),
    ExpectedValue(NodeID),
    UnexpectedToken,
    TypeNotInferred(NodeID),
    TypeMissmatch(NodeID, NodeID),
    UnlinkedNode(NodeID),
    FatalError(Ast, Box<Error>),
}

impl From<ast::Error<Any>> for Error {
    fn from(err: ast::Error<Any>) -> Self {
        Self::AstError(err)
    }
}

// // Define Ast structure and generate conversion impls
// build_ast! {
//     Storage = Storage,
//     Any [
//         Variable(VariableStorage),
//         Expression [
//             If(IfStorage),
//             Block(BlockStorage),
//             Value(ValueStorage),
//             Loop(LoopStorage),
//             Operation(OperationStorage),
//             ExpressionChain(ExpressionChainStorage),
//             FunctionCall(FunctionCallStorage),
//             FunctionDeclaration(FunctionDeclarationStorage),
//             VariableValue(VariableValueStorage),
//         ],
//         Statement [
//             VariableDeclaration(VariableDeclarationStorage),
//             TypeDeclaration(TypeDeclarationStorage),
//             Return(ReturnStorage),
//             Break(BreakStorage),
//             EvaluateExpression(EvaluateExpressionStorage),
//             VariableAssignment(VariableAssignmentStorage),
//             StaticAssignment(StaticAssignmentStorage),
//         ],
//     ]
// }

#[macro_export]
macro_rules! category_node {
    (
        Parent = $parent:ident,
        Node = $node:ident,
        Storage = $storage:ident,
        Value = $value:ident,
    ) => {
        #[derive(Debug)]
        pub struct $node;

        impl $crate::ast::NodeBody for $node {
            type Root = $parent;
            type Data = $value;
        }

        impl From<$value> for $storage {
            fn from(value: $value) -> Self {
                Storage::$node(value)
            }
        }
    };
}

#[macro_export]
macro_rules! category_trait {
    (
        Parent = $parent:ident,
        Node = $node:ident,
        Storage = $storage:ident,
        Value = $value:ident,
    ) => {
        #[derive(Debug)]
        pub trait $node {}

        impl $crate::ast::NodeBody for $node {
            type Root = $parent;
            type Data = $value;
        }

        impl From<$value> for $storage {
            fn from(value: $value) -> Self {
                Storage::$node(value)
            }
        }
    };
}

#[macro_export]
macro_rules! category_children {
    (
        Parent = $parent:ident,
        Storage = $storage:ident,
        Children = [$node:ident($value:ident), $($rest:tt)*] $(,)?
    ) => {
        $crate::category_node! {
            Parent = $parent,
            Node = $node,
            Storage = $storage,
            Value = $value,
        }
        $crate::category_node_storage! {
            Parent = $parent,
            Node = $node,
            Storage = $storage,
            Value = $value,
        }
        $crate::category_children! {
            Parent = $parent,
            Storage = $storage,
            Children = [$($rest)*],
        }
        $crate::category_node_conversion! {
            Parent = $parent,
            Node = $node,
            Storage = $storage,
        }

        // Safety: Unclear why this has to be unsafe.
        // Better (un)safe than sorry?
        unsafe impl $crate::ast::NodeData for $value {
            type Node = $node;
        }
    };

    (
        Parent = $parent:ident,
        Storage = $storage:ident,
        Children = [$node:ident, $($rest:tt)*]  $(,)?
    ) => {
        $crate::category_children! {
            Parent = $parent,
            Storage = $storage,
            Children = [$($rest)*],
        }

        impl <T> From<T> for $crate::ast::NodeID<$parent>
            where T: Into<$crate::ast::NodeID<$node>>
        {
            fn from(id: T) -> $crate::ast::NodeID<$parent> {
                let id: $crate::ast::NodeID<$node> = id.into();
                id.into()
            }
        }
    };

    (
        Parent = $parent:ident,
        Storage = $storage:ident,
        Children = [] $(,)?
    ) => {};
}

#[macro_export]
macro_rules! category_node_storage {
    (
        Parent = $parent:ident,
        Node = $node:ident,
        Storage = $storage:ident,
        Value = $value:ident,
    ) => {
        impl TryFrom<$storage> for $value {
            type Error = $crate::ast::Error<$parent>;

            fn try_from(storage: $storage) -> Result<Self, Self::Error> {
                match storage {
                    $storage::$node(value) => Ok(value),
                    _ => Err($crate::ast::Error::NodeCasting),
                }
            }
        }

        impl<'a> TryFrom<&'a $storage> for &'a $value {
            type Error = $crate::ast::Error<$parent>;

            fn try_from(storage: &'a $storage) -> Result<Self, Self::Error> {
                match storage {
                    $storage::$node(value) => Ok(value),
                    _ => Err($crate::ast::Error::NodeCasting),
                }
            }
        }

        impl<'a> TryFrom<&'a mut $storage> for &'a mut $value {
            type Error = $crate::ast::Error<$parent>;

            fn try_from(storage: &'a mut $storage) -> Result<Self, Self::Error> {
                match storage {
                    $storage::$node(value) => Ok(value),
                    _ => Err($crate::ast::Error::NodeCasting),
                }
            }
        }
    };
}

#[macro_export]
macro_rules! category_node_conversion {
    (
        Parent = $parent:ident,
        Node = $node:ident,
        Storage = $storage:ident,
    ) => {
        impl From<$crate::ast::NodeID<$node>> for $crate::ast::NodeID<$parent> {
            fn from(id: $crate::ast::NodeID<$node>) -> Self {
                $crate::ast::NodeID::new(id.inner())
            }
        }

        impl From<$crate::ast::AstNode<$node>> for $crate::ast::AstNode<$parent> {
            fn from(node: $crate::ast::AstNode<$node>) -> Self {
                // Safety: Only affects the marker type. The marker traits are
                // never trusted and real conversion checks are always executed
                // when extracting data.
                unsafe { std::mem::transmute(node) }
            }
        }

        impl<'a> From<&'a $crate::ast::AstNode<$node>> for &'a $crate::ast::AstNode<$parent> {
            fn from(node: &'a $crate::ast::AstNode<$node>) -> Self {
                // Safety: Only affects the marker type. The marker traits are
                // never trusted and real conversion checks are always executed
                // when extracting data.
                unsafe { std::mem::transmute(node) }
            }
        }

        impl<'a> From<&'a mut $crate::ast::AstNode<$node>>
            for &'a mut $crate::ast::AstNode<$parent>
        {
            fn from(node: &'a mut $crate::ast::AstNode<$node>) -> Self {
                // Safety: Only affects the marker type. The marker traits are
                // never trusted and real conversion checks are always executed
                // when extracting data.
                unsafe { std::mem::transmute(node) }
            }
        }

        impl TryFrom<$crate::ast::AstNode<$parent>> for $crate::ast::AstNode<$node> {
            type Error = $crate::ast::Error<$parent>;

            fn try_from(node: $crate::ast::AstNode<$parent>) -> Result<Self, Self::Error> {
                match node.storage() {
                    $storage::$node(_) => {
                        // Safety: Only affects the marker type. The marker traits are
                        // never trusted and real conversion checks are always executed
                        // when extracting data.
                        Ok(unsafe { std::mem::transmute(node) })
                    }
                    _ => Err($crate::ast::Error::NodeCasting),
                }
            }
        }

        impl<'a> TryFrom<&'a $crate::ast::AstNode<$parent>> for &'a $crate::ast::AstNode<$node> {
            type Error = $crate::ast::Error<$parent>;

            fn try_from(node: &'a $crate::ast::AstNode<$parent>) -> Result<Self, Self::Error> {
                match node.storage() {
                    $storage::$node(_) => {
                        // Safety: Only affects the marker type. The marker traits are
                        // never trusted and real conversion checks are always executed
                        // when extracting data.
                        Ok(unsafe { std::mem::transmute(node) })
                    }
                    _ => Err($crate::ast::Error::NodeCasting),
                }
            }
        }

        impl<'a> TryFrom<&'a mut $crate::ast::AstNode<$parent>>
            for &'a mut $crate::ast::AstNode<$node>
        {
            type Error = $crate::ast::Error<$parent>;

            fn try_from(node: &'a mut $crate::ast::AstNode<$parent>) -> Result<Self, Self::Error> {
                match node.storage() {
                    $storage::$node(_) => {
                        // Safety: Only affects the marker type. The marker traits are
                        // never trusted and real conversion checks are always executed
                        // when extracting data.
                        Ok(unsafe { std::mem::transmute(node) })
                    }
                    _ => Err($crate::ast::Error::NodeCasting),
                }
            }
        }
    };
}

#[macro_export]
macro_rules! category {
    (
        $node:ident {
            Storage = $storage:ident,
            Children = $children:tt
        }
    ) => {
        $crate::category_node! {
            Parent = $node,
            Node = $node,
            Storage = $storage,
            Value = $node,
        }

        $crate::category_children! {
            Parent = $node,
            Storage = $storage,
            Children = $children
        }

        impl ast::NodeDataStorage for $node {
            type Storage = $storage;
        }
    };
}

trait Expression: Any {}

trait Statement: Any {}

trait Any {}

// category!(Any {
//     Storage = Storage,
//     Children = [
//         Variable(VariableStorage),
//         Expression,
//     ]
// });
//
// category!(Expression {
//     Storage = Storage,
//     Children = [
//         If(IfStorage),
//         Block(BlockStorage),
//         Value(ValueStorage),
//         Loop(LoopStorage),
//         Operation(OperationStorage),
//         ExpressionChain(ExpressionChainStorage),
//         FunctionCall(FunctionCallStorage),
//         FunctionDeclaration(FunctionDeclarationStorage),
//         VariableValue(VariableValueStorage),
//     ]
// });
//
// category!(Statement {
//     Storage = Storage,
//     Children = [
//         VariableDeclaration(VariableDeclarationStorage),
//         TypeDeclaration(TypeDeclarationStorage),
//         Return(ReturnStorage),
//         Break(BreakStorage),
//         EvaluateExpression(EvaluateExpressionStorage),
//         VariableAssignment(VariableAssignmentStorage),
//         StaticAssignment(StaticAssignmentStorage),
//     ]
// });

struct Block {}

// This is how the node data is stored internally.
// Should be generated by macro
#[derive(Debug)]
pub enum Storage {
    // Expressions
    ExpressionChain(ExpressionChainStorage),
    Block(BlockStorage),
    Loop(LoopStorage),
    If(IfStorage),
    Operation(OperationStorage),
    FunctionCall(FunctionCallStorage),
    FunctionDeclaration(FunctionDeclarationStorage),
    VariableValue(VariableValueStorage),
    Value(ValueStorage),

    // Statements
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
            // Expression
                Block// | FunctionDeclaration | Loop | VariableValue |
                // Value | If | FunctionCall | ExpressionChain | Operation |
            // Statement
            //     VariableDeclaration | TypeDeclaration |
            //     Return | Break | EvaluateExpression |
            //     VariableAssignment | StaticAssignment |
            // Variable
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

impl Types for ast::AstNodeRef<dyn Any> {
    fn get_type<'this, 'ast>(
        &'this self,
        ast: &'ast Ast,
        usage: NodeUsage,
    ) -> Result<Cow<'ast, NodeType>> {
        let node = ast.get(self.id);
        reified! {node.get_type(ast, usage)}
    }
}

impl Linker for ast::AstNodeRef<dyn Any> {
    fn link(&self, ast: &mut Ast, context: LinkContext) -> Result<()> {
        let node = ast.get(self.id);
        reified! {node.link(ast, context)}
    }

    fn link_context(
        &self,
        ast: &Ast,
        child_id: impl Into<NodeID>,
        context: LinkContext,
    ) -> Result<LinkContext> {
        let node = ast.get(self.id);
        reified! {node.link_context(ast, child_id, context)}
    }
}

impl Children for ast::AstNodeRef<dyn Any> {
    fn children<'this, 'ast>(&'this self, ast: &'ast Ast) -> children::ChildIterator<'ast> {
        let node = ast.get(self.id);
        reified! {node.children(ast)}
    }
}
