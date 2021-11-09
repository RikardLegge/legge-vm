use crate::vm;
use crate::vm::{ast, transform};
use std::time;

pub struct Builder<T>
where
    T: ast::IsValid,
{
    asts: ast::Ast<T>,
    after_each_fn: &'static dyn Fn(String, time::Duration),
}

impl Builder<ast::Valid> {
    pub fn new() -> Self {
        Self {
            asts: ast::Ast::new(),
            after_each_fn: &|_, _| {},
        }
    }

    pub fn after_each(mut self, f: &'static dyn Fn(String, time::Duration)) -> Self {
        self.after_each_fn = f;
        self
    }
}

impl<T> Builder<T>
where
    T: ast::IsValid,
{
    pub fn build(self) -> ast::Ast<T> {
        self.asts
    }

    pub fn transform<N>(
        self,
        trans: &impl transform::AstTransformation<T, N>,
    ) -> crate::Result<Builder<N>>
    where
        N: ast::IsValid,
    {
        let start = vm::start_timer();
        let result = trans.transform(self.asts);
        let duration = vm::stop_timer(start);

        let name = trans.name();
        (self.after_each_fn)(name, duration);

        match result {
            Ok(asts) => Ok(Builder {
                asts,
                after_each_fn: self.after_each_fn,
            }),
            Err((asts, err)) => Err(err.into_err(asts)),
        }
    }
}
