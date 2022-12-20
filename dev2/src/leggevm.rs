use crate::node::{Result, Variable};
use crate::{checker, linker, token, tree_builder};

pub fn run() -> Result<()> {
    let tokens = token::from_chars(
        r#"
    A -> type {};
    // A.value :: 2;
    
    // test();
    
    // test :: fn () {
    // };
    
    A.func :: fn(self) -> Int {
        return 1;
    };
    
    // c := if 1 == 1 {
    //     1
    // } else loop {
    //     break 2;
    // };
    
    // loop {
    //     break 2;
    // };
     
    a := A(); 
    a.func();
    // A.func(a);
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

    let ast = tree_builder::from_tokens(tokens);
    println!("{:?}", ast);
    let (ast, root) = ast?;

    let ast = linker::link(ast, root)?;
    println!("{:?}", ast);

    // let _ast = checker::check(ast, root)?;
    Ok(())
}
