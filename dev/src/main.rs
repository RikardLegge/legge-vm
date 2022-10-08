mod ast;
mod ast_builder;
mod checker;
mod linker;
mod node;
mod token;

use std::fmt::Debug;

#[derive(Debug)]
pub enum Error {
    EOF,
    InternalError,
    VariableNotFound(String),
    ExpectedEndStatement,
    UnexpectedToken,
    TypeNotInferred(ast::NodeID),
    TypeMissmatch(ast::NodeID, ast::NodeID),
    UnlinkedNode(ast::NodeID),
    AstError(node::Ast, Box<Error>),
}

pub type Result<T, E = Error> = std::result::Result<T, E>;

fn main() -> Result<()> {
    let tokens = token::from_chars(
        r#"
    A -> type {};
    A.value :: 2;
    
    test();
    
    test :: fn () {
    };
    
    A.func :: fn(self) -> Int {
        return 1;
    };
    
    c := if 1 == 1 {
        1
    } else loop {
        break 2;
    };
    
    loop {
        break 2;
    };
     
    a := A(); 
    a.func();
    A.func(a);
    // b := a.value;
    "#,
    );
    println!(
        "[\n  {}\n]",
        tokens
            .iter()
            .map(|t| format!("{:?}", t))
            .collect::<Vec<String>>()
            .join(",\n  ")
    );

    let ast = ast_builder::from_tokens(tokens);
    println!("{:?}", ast);

    let ast = linker::link_ast(ast?)?;
    println!("{:?}", ast);

    checker::check(ast)?;
    Ok(())
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum State<Unlinked, Linked> {
    Unlinked(Unlinked),
    Linked(Linked),
}

impl<'a, Unlinked, T> TryFrom<&'a State<Unlinked, ast::NodeID<T>>> for ast::NodeID<T> {
    type Error = ();

    fn try_from(
        value: &'a State<Unlinked, ast::NodeID<T>>,
    ) -> std::result::Result<Self, Self::Error> {
        match value {
            State::Unlinked(_) => Err(()),
            State::Linked(inner) => Ok(*inner),
        }
    }
}

impl<Unlinked, Linked> From<Unlinked> for State<Unlinked, Linked> {
    fn from(value: Unlinked) -> Self {
        State::Unlinked(value)
    }
}
