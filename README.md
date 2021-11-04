# Legge Virtual Machine, attempt nr. ~8
Compiler, type-checker and stack-based virtual machine.
A bit more fun than cleaning the house this exam period.

Should probably not be used in production but that might not stop some very brave people.

## Goals
The goals of this project is to learn more about the difficulties when implementing compilers, type checkers and tree-shaking with 
strong focus on good error messages and with the goal of reasonable performance. Current single core compilation speed is around 
100K lines of code per second on an 2.6 GHz Intel Core i7 9750H for compilation, type checking and tree shaking. 

## Experiments
Since this is a project for leaning and testing out ideas, here are a few esoteric functions of the compiler.

### Side effect based tree shaker
Currently all external function are marked as having a side effect and only code which will result in a side effect
will be run. This means that the variable a will be removed in the following trivial case.
```rust
a :: 1;
exit(10);
```

A less trivial case is for example one using closures. In the following example `value` is marked as causing a side effect
if written since it is read in `exit`, resulting in `inc` being marked as having a side effect since it writes to `value`.
Many known bugs still exist, a trivial way to break the tree shaker is using conditional expressions. 
```rust
value := 1;
hang :: fn() {loop {}}
inc :: fn() {value = value + 1}
end :: fn() {exit(value);}

inc();
hang(); // This is never run since it's side effect free!
end();
```

### Automatic code fixer
The parser is written in a way where it is always able to recommend a character to insert
or replace to make the code runnable. Currently the grammar is set up in a way where the tokenizer never fails
for a sequence of valid characters, and since the parser is always able to give modification recommendation 
which makes it parse one more token, it is possible to ensure that any set of characters automatically can 
be transformed into a valid AST. 
For example `A ->` -> `A -> type` -> `A -> type {` -> `A -> type {}` which results in a valid program.

Allowing this for the type checker would be more of a challenge but should not be impossible, the 
recommendations are likely to be worse than for the parser but would allow the compiler to compile
and run any sequence of valid characters which can be good from an educational point of view. 

## Features
- [x] Compiler (Tokenizer -> Ast -> Bytecode)
- [x] Interpreter
- [x] Type checker
- [x] Custom function declarations
- [x] Closures
- [x] External functions
- [x] Bugs
- [x] Side effect based tree shaker
- [x] Custom types
- [x] A few tests showcasing the functionality
- [ ] Multi thread compilation
- [ ] Decouple runtime from compiler
- [ ] Test to se if it actually works

## Example 1: Overview
```rust
// This is a comment

// We can declare custom types
A -> type {
   value: int
}

// We can declare nested types, but nesting B in A would result in an error.
B -> type {
   a: A
}

// Support for closures and higher order functions
factory :: fn (new_a: Fn(int) -> A) -> Fn(int) -> B {
    return fn(value: int) -> B {
        b := B();
        b.a = new_a(value);
        return b;
    };
}

// Type inference ensures that the type of new becomes
//   new : Fn(int) -> B
// and is a callable function.
new :: factory(fn(v: int) -> A {
    a := A();
    a.value = v;
    return a;
});

// Allow importing foreign functions. Currently all imports are hard coded in the runtime.
import std.exit;

// Allows defining mutable 
mutable := 1;
mutable = 2;

// And non mutable variables 
unmutable :: 1;
// unmutable = 2; // Compilation error

// This will compile and run as expected
exit(new(42));
// exit(new("")); // Compilation error  since new : Fn(int) -> B
```

## Example 2: Error messages
```rust
A -> type {
   value: int
}
B -> type {
   a: A
}
factory :: fn (new_a: Fn(int) -> A) -> Fn(int) -> B {
    return fn(value: int) -> B {
        b := B();
        b.a = new_a(value);
        return b;
    };
}
new :: factory(fn(v: int) -> A {
    a := A();
    a.value = v;
    return a;
});
import std.exit;
exit(new(""));
```

### Error message
```rust
Ast Error: function arguments are of the wrong type, int expected, string provided
   4 |     B -> type {
   5 |        a: A
   6 |     }
   7 |     factory :: fn (new_a: Fn(int) -> A) -> Fn(int) -> B {
                                                     ^^^ Expected argument type

  11 |             return b;
  12 |         };
  13 |     }
  14 |     new :: factory(fn(v: int) -> A {
                  ^^^^^^^^^^^^^^^^^^^^^^^^^ From return value of function

  17 |         return a;
  18 |     });
  19 |     import std.exit;
  20 |     exit(new(""));
                    ^^ Provided argument
```