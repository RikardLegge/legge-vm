# Legge Virtual Machine, attempt nr. ~8
Compiler, type-checker and stack-based virtual machine.
A bit more fun than cleaning the house this exam period.

Should probably not be used in production but that might not stop some very brave people.

## Examples
```
// This is a comment
name :: "Staticly Declared string with infered type"

curry :: fn(f: Fn(string, string), v1: string) -> Fn(string) {
    return fn(v2: string) {
        f(v1, v2);
    }
}

print_context :: fn(context: string, message: string) {
    // Import function with side effect.
    import log;
    log(context, message);
}

print(name);

```

## Roadmap
- [x] Compiler (Tokenizer -> Ast -> Bytecode)
- [x] Interpreter
- [x] Type checker
- [x] Custom function declarations
- [x] Closures
- [x] External functions
- [x] Side effect based tree shaker
- [x] Bugs
- [x] Custom types
- [x] A few tests showcasing the functionality
- [ ] Test to se if it actually works

## Benchmarks 

From our super scientific benchmarks looping in Legge VM is about 100x slower than looping in pure rust.