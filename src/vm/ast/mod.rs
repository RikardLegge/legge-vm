mod ast;
mod error;
mod nodebody;
mod state;

use std::result;

pub use ast::{
    Ast, AstBranch, AstBranchID, DebugSymbols, InferredType, Node, NodeID, NodeReference,
    NodeReferenceLocation, NodeReferenceType, NodeType, NodeTypeSource, NodeValue,
    PartialNodeValue, PartialType, ProcedureDeclarationNode, SideEffect,
};
pub use error::{Err, ErrPart};
pub use nodebody::*;
pub use state::{Empty, Invalid, Linked, TypesChecked, TypesInferred, Valid};
pub use state::{IsEmpty, IsInvalid, IsLinked, IsTypesChecked, IsTypesInferred, IsValid};

pub type Result<N = NodeID> = result::Result<N, Err>;
