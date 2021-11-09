mod ast;
mod error;
mod nodebody;
mod state;
mod transform;

use std::result;

pub use ast::{
    Ast, AstBranch, AstBranchID, DebugSymbols, InferredType, Node, NodeID, NodeReference,
    NodeReferenceLocation, NodeReferenceType, NodeType, NodeTypeSource, NodeValue,
    PartialNodeValue, PartialType, ProcedureDeclarationNode, SideEffect,
};
pub use error::{Err, ErrPart};
pub use nodebody::*;
pub use state::{Invalid, Linked, TypesChecked, TypesInferred, Valid};
pub use state::{IsInvalid, IsLinked, IsTypesChecked, IsTypesInferred, IsValid};
pub use transform::*;

pub type Result<N = NodeID> = result::Result<N, Err>;
