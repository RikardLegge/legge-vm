# Legge Virtual Machine, attempt nr. ~8
Compiler, type-checker and stack-based virtual machine.
A bit more fun than cleaning the house this exam period.

Should probably not be used in production but that might not stop some very brave people.

## Examples
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

## Roadmap
- [x] Compiler (Tokenizer -> Ast -> Bytecode)
- [x] Interpreter
- [x] Type checker
- [x] Custom function declarations
- [x] Closures
- [x] External functions
- [x] Side effect based tree shaker
- [x] Custom types
- [x] A few tests showcasing the functionality
- [x] Bugs
- [ ] Test to se if it actually works

## Benchmarks 

From our super scientific benchmarks looping in Legge VM is about 100x slower than looping in pure rust.