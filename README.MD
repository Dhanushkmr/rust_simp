# SIMP COMPILER IN RUST!

## What is this?
This is a compiler for a language called SIMP. It is a simple language that is used to demonstrate the basics of compiler design. It is a subset of the C language.
This compiler was originally written in Scala3 with JVM as the target. This is a rewrite of the compiler in Rust with LLVM as the target.

## How to use?
### Prerequisites
- Rust
- LLVM
- GCC


### Building
- Clone the repo
- Run `cargo build --release`
- The executable will be in `target/release/simp`
- Run `./target/release/simp <path to source file>` you can use simp.txt as an example
- The output will be in `output.ll`
- Run `llc output.ll` to generate the assembly
- Run `as output.s -o output.o` to generate the object file
- Run `gcc output.o -o output` to generate the executable
- Run `./output` to run the executable
- Run `echo $?` to see the exit code (this is the return value of the main function) the example should return 5

I know its a lot of steps, but I will automate this in the future.

## Pending


- Detailed blog posts coming soon!
- Better error handling for lexer and parser (make use of SrcLoc)
- More semantic checks (Name resolution, CFG)
- Trying out the performance optimizations in LLVM
- Better error handling for LLVM
- Getting rid of the nooby .unwrap() 
- Better error messages

Credits: Kenny Zhuo Ming Lu, for the original compiler in Scala3