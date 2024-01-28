use std::env;
use std::fs;

use inkwell::context::Context;
use ir::llvm_ir::Compiler;

pub mod interpreter;
pub mod ir;
pub mod semantic;
pub mod syntax;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];

    // Read the input file
    let input = fs::read_to_string(filename).unwrap();

    // Lex and parse the input into an AST
    let (_, tokens) = syntax::lexer::parse_tokens(&input).unwrap();
    let (_, ast) = syntax::parser::parse_stmts(&tokens).unwrap();

    // Perform semantic analysis on the AST
    semantic::type_inf::type_inf(ast.clone())?;

    // Interpret the AST directly
    // TODO read the input from stdine
    // let result = interpreter::simp_big_step::interpret(&ast, 0)?;
    // println!("Interpreter result: {:?}", result);

    // Or compile the AST into LLVM IR
    let context = Context::create();
    let mut compiler = Compiler::new(&context);
    compiler.compile_program(&ast)?;
    compiler.module.print_to_file("output.ll")?;

    Ok(())
}
