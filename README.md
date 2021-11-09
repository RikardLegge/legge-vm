# Legge Virtual Machine
Compiler, type-checker and stack-based virtual machine used as a learning project into how compilers
work. This is also a project used to implement interesting ideas and understand why they are not more common.

The compiler and VM should probably not be used in production but that might not stop some very brave people.
No guarantees are given and there are many known bugs which might cause subtle errors or crasher
for arbitrary code.

## Goals
The goals of this project is to learn more about the difficulties when implementing compilers, type checkers and tree-shaking with
strong focus on good error messages and fast compilation speed.

Ease of development is a core part of the project since it changes rapidly. Rust's type system helps define what guarantees 
are provided by each step of the pipeline and are used to give clear error messages when assumptions are no longer upheld.

Good error messages are provided by integrating hints at each point the compilation might fail. 
This is an area which still has a lot of room for improvement, but as the examples further down show, 
it currently is able to show how it reasons about type for example.

To ensure fast compilation speeds the grammar and language is designed in such a way that each source file is a compilation unit.
The focus on compilation speed is partially inspired by [esbuild](https://github.com/evanw/esbuild) which is a clear example of how 
today's machines should be able to compile code in an instant, at least without applying too many optimizations.
Except for some orchestration, all steps for each compilation unit can be run in parallel with (in the worst case scenario) 
read only access between units. 
Since the information about what information other compilation units require, like inferred types or usage information, 
can be computed at an early stage, there are still improvements to be made. These improvements include replacing some locks with
more well though out data structures. 

### Performance
Current multi-core compilation speed of a synthetic semi-trivial dataset is around 1M source lines of code in per 1.4 seconds on
an 2.6 GHz Intel Core i7 9750H with 8 hyper threaded cores.
The benchmark is run over a set of 300 files each file importing 4 other file, and time keeping starts when the entrypoint
starts parsing and ends when the bytecode has completed generating.

## Features
- [x] Language features
    - [x] Custom types
    - [x] Custom function declarations
    - [x] Closures
    - [x] External functions
    - [x] Importing constants, types and functions from other files
- [x] Compile to runnable bytecode
- [x] Multi threading (Parsing, variable resolution, type inference, type checker, bytecode generation)
- [x] Bytecode interpreter
- [x] Bugs
- [x] A few tests showcasing the functionality

### TODO
- [ ] Add more test cases
- [ ] Implement bare-bone standard library
- [ ] Decouple runtime from compiler

## Example code
The language takes much inspiration from rust which can clearly be seen in the following code.

```rust
// This is a comment

// Declaration of custom type, read as A constructs type { ... }
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

## Experiments
Since this is a project for learning and testing out ideas, here are a few exotic functions of the compiler which either
has been tested or is currently under development.

### 1. Type inference without type hints. [Discontinued]
When first developing the type inference system, before adding type hints, the type information
originated at variable assignments and flowed through the rest of the source graph.
The issue which immediately became apparent was that changing a variable or an operation
in one method could cause the types inferred inside a seemingly unrelated method to be 
changed and resulting in an error. 
This spooky action at a distance showed that there must be limits to how type information
is allowed to flow.

### 2. Side effect based tree shaker [WIP]
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
import std.exit;
value := 1;
hang :: fn() {loop {}}
inc :: fn() {value = value + 1;}
end :: fn() {exit(value);}

inc();
hang(); // This is never run since it's side effect free!
end();
```

### 3. Helpful error messages [WIP]

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
// Error: new takes an int!
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

### 4. Automatic code fixer [Planned]
The parser is written in a way where it is always able to recommend a character to insert
or replace to make the code runnable. Currently the grammar is set up in a way where the tokenizer never fails
for a sequence of valid characters, and since the parser is always able to give modification recommendation
which makes it parse one more token, it is possible to ensure that any set of characters automatically can
be transformed into a valid AST.
For example `A ->` -> `A -> type` -> `A -> type {` -> `A -> type {}` which results in a valid program.

Allowing this for the type checker would be more of a challenge but should not be impossible, the
recommendations are likely to be worse than for the parser but would allow the compiler to compile
and run any sequence of valid characters which can be good from an educational point of view. 
