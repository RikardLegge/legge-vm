mod builder;
mod check_transform;
mod infertype_transform;
mod link_transform;
mod main_transform;
mod token_to_ast;
mod treeshake_transform;

pub use builder::Builder;
pub use check_transform::CheckTypes;
pub use infertype_transform::InferTypes;
pub use link_transform::Link;
pub use main_transform::Main;
pub use treeshake_transform::TreeShake;

use crate::vm::ast;
use crate::vm::ast::{Invalid, IsValid};
use std::result;

pub type Result<T, E = Invalid> = result::Result<ast::Ast<T>, (ast::Ast<E>, ast::Err)>;

pub trait AstTransformation<F, T>
where
    F: IsValid,
    T: IsValid,
{
    fn name(&self) -> String;

    fn transform(&self, ast: ast::Ast<F>) -> Result<T>;
}
