# Legge Virtual Machine, attempt nr. ~8
Compilter, typechecker and stackbased virtual machine.
A bit more fun than cleaning the house this exam period.

Should probably not be used in production but that might not stop some very brave people.

## Roadmap
- [x] Compiler (Tokenizer -> Ast -> Bytecode)
- [x] Interpreter
- [x] Type checker
- [x] Custom function declarations
- [x] Closures
- [x] External functions
- [x] Bugs
- [ ] Custom types

## Benchmarks 

From our super scientific benchmarks looping in Legge VM is about 100x slower than looping in pure rust.