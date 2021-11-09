use crate::vm::ast::{AstTransformation, IsValid, Valid};
use crate::vm::{ast, debug};
use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use std::time;

pub struct TransformDebugInfo(HashMap<String, time::Duration>);

impl Debug for TransformDebugInfo {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for (k, v) in &self.0 {
            write!(f, "{}: {:?}\n", k, v)?;
        }
        Ok(())
    }
}

pub struct TransformBuilder<T>
where
    T: IsValid,
{
    asts: ast::Ast<T>,
    time: HashMap<String, time::Duration>,
}

impl TransformBuilder<Valid> {
    pub fn new() -> Self {
        Self {
            asts: ast::Ast::new(),
            time: HashMap::new(),
        }
    }
}

impl<T> TransformBuilder<T>
where
    T: IsValid,
{
    pub fn build(self) -> (ast::Ast<T>, TransformDebugInfo) {
        (self.asts, TransformDebugInfo(self.time))
    }

    pub fn transform<N>(
        self,
        trans: &impl AstTransformation<T, N>,
    ) -> crate::Result<TransformBuilder<N>>
    where
        N: IsValid,
    {
        let start = debug::start_timer();
        let result = trans.transform(self.asts);
        let duration = debug::stop_timer(start);

        let name = trans.name();
        let mut time = self.time;
        time.insert(name, duration);
        match result {
            Ok(asts) => Ok(TransformBuilder { asts, time }),
            Err((asts, err)) => Err(err.into_err(asts)),
        }
    }
}
