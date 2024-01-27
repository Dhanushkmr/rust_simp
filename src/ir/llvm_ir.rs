use inkwell::module::Module;
use inkwell::values::IntValue;
use inkwell::IntPredicate;
use inkwell::{builder::BuilderError, context::Context};
use std::collections::HashMap;

use crate::syntax::ast::{Const, Exp, Stmt, Var};

pub struct Compiler<'ctx> {
    context: &'ctx Context,
    builder: inkwell::builder::Builder<'ctx>,
    variables: HashMap<String, IntValue<'ctx>>,
    module: Module<'ctx>,
}

impl<'ctx> Compiler<'ctx> {
    pub fn new(context: &'ctx Context) -> Self {
        let builder = context.create_builder();
        let module = context.create_module("main");

        Self {
            context,
            builder,
            variables: std::collections::HashMap::new(),
            module,
        }
    }

    pub fn compile_exp(&mut self, exp: Exp) -> Result<IntValue<'ctx>, BuilderError> {
        match exp {
            Exp::Plus { e1, e2 } => {
                let e1_value = self.compile_exp(*e1)?;
                let e2_value = self.compile_exp(*e2)?;
                self.builder.build_int_add(e1_value, e2_value, "add")
            }
            Exp::Minus { e1, e2 } => {
                let e1_value = self.compile_exp(*e1)?;
                let e2_value = self.compile_exp(*e2)?;
                self.builder.build_int_sub(e1_value, e2_value, "sub")
            }
            Exp::Mult { e1, e2 } => {
                let e1_value = self.compile_exp(*e1)?;
                let e2_value = self.compile_exp(*e2)?;
                self.builder.build_int_mul(e1_value, e2_value, "mul")
            }
            Exp::DEqual { e1, e2 } => {
                let e1_value = self.compile_exp(*e1)?;
                let e2_value = self.compile_exp(*e2)?;
                self.builder
                    .build_int_compare(IntPredicate::EQ, e1_value, e2_value, "eq")
            }
            Exp::LThan { e1, e2 } => {
                let e1_value = self.compile_exp(*e1)?;
                let e2_value = self.compile_exp(*e2)?;
                self.builder
                    .build_int_compare(IntPredicate::ULT, e1_value, e2_value, "lt")
            }
            Exp::ConstExp { l } => match l {
                Const::IntConst { v } => Ok(self.context.i32_type().const_int(v as u64, false)),
                Const::BoolConst { v } => Ok(self
                    .context
                    .bool_type()
                    .const_int(if v { 1 } else { 0 }, false)),
            },
            Exp::VarExp { v } => self
                .variables
                .get(&v.0)
                .cloned()
                .ok_or(BuilderError::ExtractOutOfRange),
            Exp::ParenExp { e } => self.compile_exp(*e),
        }
    }

    pub fn compile_stmt(&mut self, stmt: Stmt) -> Result<(), Box<dyn std::error::Error>> {
        match stmt {
            Stmt::Assign { x, e } => {
                let value = self.compile_exp(e)?;
                self.variables.insert(x.0, value);
                Ok(())
            }
            Stmt::If { cond, then, el } => {
                let cond_value = self.compile_exp(cond)?;
                println!("{:?}", cond_value);
                let then_block = self.context.append_basic_block(
                    self.builder
                        .get_insert_block()
                        .unwrap()
                        .get_parent()
                        .unwrap(),
                    "then",
                );
                let else_block = self.context.append_basic_block(
                    self.builder
                        .get_insert_block()
                        .unwrap()
                        .get_parent()
                        .unwrap(),
                    "else",
                );
                let cont_block = self.context.append_basic_block(
                    self.builder
                        .get_insert_block()
                        .unwrap()
                        .get_parent()
                        .unwrap(),
                    "cont",
                );

                self.builder
                    .build_conditional_branch(cond_value, then_block, else_block)?;

                self.builder.position_at_end(then_block);
                for stmt in then {
                    self.compile_stmt(stmt)?;
                }
                self.builder.build_unconditional_branch(cont_block)?;

                self.builder.position_at_end(else_block);
                for stmt in el {
                    self.compile_stmt(stmt)?;
                }
                self.builder.build_unconditional_branch(cont_block)?;

                self.builder.position_at_end(cont_block);
                Ok(())
            }
            Stmt::Nop => Ok(()),
            Stmt::While { cond, body } => {
                let cond_block = self.context.append_basic_block(
                    self.builder
                        .get_insert_block()
                        .unwrap()
                        .get_parent()
                        .unwrap(),
                    "cond",
                );
                let body_block = self.context.append_basic_block(
                    self.builder
                        .get_insert_block()
                        .unwrap()
                        .get_parent()
                        .unwrap(),
                    "body",
                );
                let end_block = self.context.append_basic_block(
                    self.builder
                        .get_insert_block()
                        .unwrap()
                        .get_parent()
                        .unwrap(),
                    "end",
                );

                // Start with a branch to the condition block
                self.builder.build_unconditional_branch(cond_block)?;

                // Condition block
                self.builder.position_at_end(cond_block);
                let cond_value = self.compile_exp(cond)?;
                self.builder
                    .build_conditional_branch(cond_value, body_block, end_block)?;

                // Body block
                self.builder.position_at_end(body_block);
                for stmt in body {
                    self.compile_stmt(stmt)?;
                }
                // After the body, branch back to the condition
                self.builder.build_unconditional_branch(cond_block)?;

                // End block
                self.builder.position_at_end(end_block);
                Ok(())
            }
            Stmt::Ret { x } => {
                let value = self
                    .variables
                    .get(&x.0)
                    .ok_or("variable not found")?
                    .clone();
                self.builder.build_return(Some(&value))?;
                Ok(())
            }
        }
    }

    pub fn compile_program(&mut self, program: &[Stmt]) -> Result<(), Box<dyn std::error::Error>> {
        let i32_type = self.context.i32_type();
        let fn_type = i32_type.fn_type(&[], false);
        let function = self.module.add_function("main", fn_type, None);
        let entry_block = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry_block);

        for stmt in program {
            self.compile_stmt(stmt.clone())?;
        }

        // If the program doesn't end with a return statement, add a default return
        if let Some(Stmt::Ret { .. }) = program.last() {
            // The program ends with a return statement, so we don't need to add anything
        } else {
            self.builder
                .build_return(Some(&i32_type.const_int(0, false)))?;
        }

        if function.verify(true) {
            Ok(())
        } else {
            Err("LLVM function verification failed".into())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use inkwell::execution_engine::JitFunction;
    use inkwell::OptimizationLevel;

    type CompileExpFn = unsafe extern "C" fn() -> i32;

    #[test]
    fn test_compile_exp_plus() {
        let context = Context::create();
        let mut compiler = Compiler::new(&context);

        let exp = Exp::Plus {
            e1: Box::new(Exp::ConstExp {
                l: Const::IntConst { v: 2 },
            }),
            e2: Box::new(Exp::ConstExp {
                l: Const::IntConst { v: 3 },
            }),
        };

        // Create function
        let fn_type = compiler.context.i32_type().fn_type(&[], false);
        let function = compiler.module.add_function("main", fn_type, None);

        // Define a basic block
        let basic_block = compiler.context.append_basic_block(function, "entry");

        // Set the insertion point to this basic block
        compiler.builder.position_at_end(basic_block);

        let compiled_exp = compiler.compile_exp(exp).unwrap();
        let _ = compiler.builder.build_return(Some(&compiled_exp));

        let execution_engine = compiler
            .module
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();
        let main: JitFunction<CompileExpFn> =
            unsafe { execution_engine.get_function("main").unwrap() };
        let result = unsafe { main.call() };

        assert_eq!(result, 5);
    }

    #[test]
    fn test_compile_exp_minus() {
        let context = Context::create();
        let mut compiler = Compiler::new(&context);

        let exp = Exp::Minus {
            e1: Box::new(Exp::ConstExp {
                l: Const::IntConst { v: 4 },
            }),
            e2: Box::new(Exp::ConstExp {
                l: Const::IntConst { v: 3 },
            }),
        };

        // Create function
        let fn_type = compiler.context.i32_type().fn_type(&[], false);
        let function = compiler.module.add_function("main", fn_type, None);

        // Define a basic block
        let basic_block = compiler.context.append_basic_block(function, "entry");

        // Set the insertion point to this basic block
        compiler.builder.position_at_end(basic_block);

        let compiled_exp = compiler.compile_exp(exp).unwrap();
        let _ = compiler.builder.build_return(Some(&compiled_exp));

        let execution_engine = compiler
            .module
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();
        let main: JitFunction<CompileExpFn> =
            unsafe { execution_engine.get_function("main").unwrap() };
        let result = unsafe { main.call() };

        assert_eq!(result, 1);
    }

    #[test]
    fn test_compile_exp_mult() {
        let context = Context::create();
        let mut compiler = Compiler::new(&context);

        let exp = Exp::Mult {
            e1: Box::new(Exp::ConstExp {
                l: Const::IntConst { v: 2 },
            }),
            e2: Box::new(Exp::ConstExp {
                l: Const::IntConst { v: 4 },
            }),
        };

        // Create function
        let fn_type = compiler.context.i32_type().fn_type(&[], false);
        let function = compiler.module.add_function("main", fn_type, None);

        // Define a basic block
        let basic_block = compiler.context.append_basic_block(function, "entry");

        // Set the insertion point to this basic block
        compiler.builder.position_at_end(basic_block);

        let compiled_exp = compiler.compile_exp(exp).unwrap();
        let _ = compiler.builder.build_return(Some(&compiled_exp));

        let execution_engine = compiler
            .module
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();
        let main: JitFunction<CompileExpFn> =
            unsafe { execution_engine.get_function("main").unwrap() };
        let result = unsafe { main.call() };

        assert_eq!(result, 8);
    }

    #[test]
    fn test_compile_exp_de() {
        let context = Context::create();
        let mut compiler = Compiler::new(&context);

        let exp = Exp::DEqual {
            e1: Box::new(Exp::ConstExp {
                l: Const::IntConst { v: 2 },
            }),
            e2: Box::new(Exp::ConstExp {
                l: Const::IntConst { v: 2 },
            }),
        };

        // Create function
        let fn_type = compiler.context.i32_type().fn_type(&[], false);
        let function = compiler.module.add_function("main", fn_type, None);

        // Define a basic block
        let basic_block = compiler.context.append_basic_block(function, "entry");

        // Set the insertion point to this basic block
        compiler.builder.position_at_end(basic_block);

        let compiled_exp = compiler.compile_exp(exp).unwrap();
        let _ = compiler.builder.build_return(Some(&compiled_exp));

        let execution_engine = compiler
            .module
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();
        let main: JitFunction<CompileExpFn> =
            unsafe { execution_engine.get_function("main").unwrap() };
        let result = unsafe { main.call() };

        assert_eq!(result, 1);
    }

    #[test]
    fn test_compile_exp_le() {
        let context = Context::create();
        let mut compiler = Compiler::new(&context);

        let exp = Exp::LThan {
            e1: Box::new(Exp::ConstExp {
                l: Const::IntConst { v: 2 },
            }),
            e2: Box::new(Exp::ConstExp {
                l: Const::IntConst { v: 4 },
            }),
        };

        // Create function
        let fn_type = compiler.context.i32_type().fn_type(&[], false);
        let function = compiler.module.add_function("main", fn_type, None);

        // Define a basic block
        let basic_block = compiler.context.append_basic_block(function, "entry");

        // Set the insertion point to this basic block
        compiler.builder.position_at_end(basic_block);

        let compiled_exp = compiler.compile_exp(exp).unwrap();
        let _ = compiler.builder.build_return(Some(&compiled_exp));

        let execution_engine = compiler
            .module
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();
        let main: JitFunction<CompileExpFn> =
            unsafe { execution_engine.get_function("main").unwrap() };
        let result = unsafe { main.call() };

        assert_eq!(result, 1);
    }

    #[test]
    fn test_compile_exp_paren() {
        let context = Context::create();
        let mut compiler = Compiler::new(&context);

        let exp = Exp::ParenExp {
            e: Box::new(Exp::Mult {
                e1: Box::new(Exp::ConstExp {
                    l: Const::IntConst { v: 2 },
                }),
                e2: Box::new(Exp::ConstExp {
                    l: Const::IntConst { v: 4 },
                }),
            }),
        };

        // Create function
        let fn_type = compiler.context.i32_type().fn_type(&[], false);
        let function = compiler.module.add_function("main", fn_type, None);

        // Define a basic block
        let basic_block = compiler.context.append_basic_block(function, "entry");

        // Set the insertion point to this basic block
        compiler.builder.position_at_end(basic_block);

        let compiled_exp = compiler.compile_exp(exp).unwrap();
        let _ = compiler.builder.build_return(Some(&compiled_exp));

        let execution_engine = compiler
            .module
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();
        let main: JitFunction<CompileExpFn> =
            unsafe { execution_engine.get_function("main").unwrap() };
        let result = unsafe { main.call() };

        assert_eq!(result, 8);
    }

    #[test]
    fn test_compile_exp_compound() {
        let context = Context::create();
        let mut compiler = Compiler::new(&context);

        let exp = Exp::ParenExp {
            e: Box::new(Exp::Plus {
                e1: Box::new(Exp::ConstExp {
                    l: Const::IntConst { v: 2 },
                }),
                e2: Box::new(Exp::Mult {
                    e1: Box::new(Exp::ConstExp {
                        l: Const::IntConst { v: 2 },
                    }),
                    e2: Box::new(Exp::ConstExp {
                        l: Const::IntConst { v: 3 },
                    }),
                }),
            }),
        };

        // Create function
        let fn_type = compiler.context.i32_type().fn_type(&[], false);
        let function = compiler.module.add_function("main", fn_type, None);

        // Define a basic block
        let basic_block = compiler.context.append_basic_block(function, "entry");

        // Set the insertion point to this basic block
        compiler.builder.position_at_end(basic_block);

        let compiled_exp = compiler.compile_exp(exp).unwrap();
        let _ = compiler.builder.build_return(Some(&compiled_exp));

        let execution_engine = compiler
            .module
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();
        let main: JitFunction<CompileExpFn> =
            unsafe { execution_engine.get_function("main").unwrap() };
        let result = unsafe { main.call() };

        assert_eq!(result, 8);
    }

    #[test]
    fn test_compile_stmt_assign() {
        let context = Context::create();
        let mut compiler = Compiler::new(&context);

        let stmt = Stmt::Assign {
            x: Var("x".to_string()),
            e: Exp::ParenExp {
                e: Box::new(Exp::Plus {
                    e1: Box::new(Exp::ConstExp {
                        l: Const::IntConst { v: 2 },
                    }),
                    e2: Box::new(Exp::Mult {
                        e1: Box::new(Exp::ConstExp {
                            l: Const::IntConst { v: 2 },
                        }),
                        e2: Box::new(Exp::ConstExp {
                            l: Const::IntConst { v: 3 },
                        }),
                    }),
                }),
            },
        };

        // Create function
        let fn_type = compiler.context.i32_type().fn_type(&[], false);
        let function = compiler.module.add_function("main", fn_type, None);

        // Define a basic block
        let basic_block = compiler.context.append_basic_block(function, "entry");

        // Set the insertion point to this basic block
        compiler.builder.position_at_end(basic_block);

        compiler.compile_stmt(stmt).unwrap();

        // Add return statement
        let return_value = *compiler.variables.get("x").unwrap();
        compiler.builder.build_return(Some(&return_value)).unwrap();

        let execution_engine = compiler
            .module
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();
        let main: JitFunction<CompileExpFn> =
            unsafe { execution_engine.get_function("main").unwrap() };
        let result = unsafe { main.call() };

        assert_eq!(result, 8);
    }

    #[test]
    fn test_compile_stmt_while() {
        let context = Context::create();
        let mut compiler = Compiler::new(&context);

        let stmt = Stmt::While {
            cond: Exp::LThan {
                e1: Box::new(Exp::VarExp {
                    v: Var("x".to_string()),
                }),
                e2: Box::new(Exp::ConstExp {
                    l: Const::IntConst { v: 5 },
                }),
            },
            body: vec![Stmt::Assign {
                x: Var("x".to_string()),
                e: Exp::Plus {
                    e1: Box::new(Exp::VarExp {
                        v: Var("x".to_string()),
                    }),
                    e2: Box::new(Exp::ConstExp {
                        l: Const::IntConst { v: 1 },
                    }),
                },
            }],
        };

        // Create function
        let fn_type = compiler.context.i32_type().fn_type(&[], false);
        let function = compiler.module.add_function("main", fn_type, None);

        // Define a basic block
        let basic_block = compiler.context.append_basic_block(function, "entry");

        // Set the insertion point to this basic block
        compiler.builder.position_at_end(basic_block);

        // Initialize variable x to 0
        compiler.variables.insert(
            "x".to_string(),
            compiler.context.i32_type().const_int(0, false),
        );

        compiler.compile_stmt(stmt).unwrap();

        // Add return statement
        let return_value = *compiler.variables.get("x").unwrap();
        compiler.builder.build_return(Some(&return_value)).unwrap();

        let execution_engine = compiler
            .module
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();
        let main: JitFunction<CompileExpFn> =
            unsafe { execution_engine.get_function("main").unwrap() };
        let result = unsafe { main.call() };

        assert_eq!(result, 5);
    }

    #[test]
    fn test_compile_stmt_if() {
        let context = Context::create();
        let mut compiler = Compiler::new(&context);

        let stmt = Stmt::If {
            cond: Exp::DEqual {
                e1: Box::new(Exp::VarExp {
                    v: Var("x".to_string()),
                }),
                e2: Box::new(Exp::ConstExp {
                    l: Const::IntConst { v: 5 },
                }),
            },
            then: vec![Stmt::Assign {
                x: Var("x".to_string()),
                e: Exp::Plus {
                    e1: Box::new(Exp::VarExp {
                        v: Var("x".to_string()),
                    }),
                    e2: Box::new(Exp::ConstExp {
                        l: Const::IntConst { v: 1 },
                    }),
                },
            }],
            el: vec![Stmt::Assign {
                x: Var("x".to_string()),
                e: Exp::Minus {
                    e1: Box::new(Exp::VarExp {
                        v: Var("x".to_string()),
                    }),
                    e2: Box::new(Exp::ConstExp {
                        l: Const::IntConst { v: 1 },
                    }),
                },
            }],
        };

        // Create function
        let fn_type = compiler.context.i32_type().fn_type(&[], false);
        let function = compiler.module.add_function("main", fn_type, None);

        // Define a basic block
        let basic_block = compiler.context.append_basic_block(function, "entry");

        // Set the insertion point to this basic block
        compiler.builder.position_at_end(basic_block);

        // Allocate `x` on the stack at the beginning of your function.
        let x_ptr = compiler
            .builder
            .build_alloca(compiler.context.i32_type(), "x")
            .unwrap();

        // Store the initial value into `x` (assuming the initial value is 5).
        compiler
            .builder
            .build_store(x_ptr, compiler.context.i32_type().const_int(5, false))
            .unwrap();

        // // Initialize variable x to 5
        // compiler.variables.insert(
        //     "x".to_string(),
        //     compiler.context.i32_type().const_int(5, false),
        // );

        compiler.compile_stmt(stmt).unwrap();
        let return_value = compiler
            .builder
            .build_load(compiler.context.i32_type(), x_ptr, "x")
            .unwrap()
            .into_int_value();

        // Add return statement
        // let return_value = *compiler.variables.get("x").unwrap();
        compiler.builder.build_return(Some(&return_value)).unwrap();

        let execution_engine = compiler
            .module
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();
        let main: JitFunction<CompileExpFn> =
            unsafe { execution_engine.get_function("main").unwrap() };
        let result = unsafe { main.call() };
        println!("{}", compiler.module.print_to_string().to_string());
        assert_eq!(result, 6);
    }

    #[test]
    fn test_llvm_maker() {
        let context = Context::create();
        let mut compiler = Compiler::new(&context);

        let x = Var("x".to_string());
        let s = Var("s".to_string());
        let c = Var("c".to_string());
        let p = vec![
            Stmt::Assign {
                x: x.clone(),
                e: Exp::ConstExp {
                    l: Const::IntConst { v: 3 },
                },
            },
            Stmt::Assign {
                x: s.clone(),
                e: Exp::ConstExp {
                    l: Const::IntConst { v: 0 },
                },
            },
            Stmt::Assign {
                x: c.clone(),
                e: Exp::ConstExp {
                    l: Const::IntConst { v: 0 },
                },
            },
            Stmt::While {
                cond: Exp::LThan {
                    e1: Box::new(Exp::VarExp { v: c.clone() }),
                    e2: Box::new(Exp::VarExp { v: x.clone() }),
                },
                body: vec![
                    Stmt::Assign {
                        x: s.clone(),
                        e: Exp::Plus {
                            e1: Box::new(Exp::VarExp { v: c.clone() }),
                            e2: Box::new(Exp::VarExp { v: s.clone() }),
                        },
                    },
                    Stmt::Assign {
                        x: c.clone(),
                        e: Exp::Plus {
                            e1: Box::new(Exp::VarExp { v: c.clone() }),
                            e2: Box::new(Exp::ConstExp {
                                l: Const::IntConst { v: 1 },
                            }),
                        },
                    },
                ],
            },
            Stmt::Ret { x: s.clone() },
        ];

        // Create function
        let fn_type = compiler.context.i32_type().fn_type(&[], false);
        let function = compiler.module.add_function("main", fn_type, None);

        // Define a basic block
        let basic_block = compiler.context.append_basic_block(function, "entry");

        // Set the insertion point to this basic block
        compiler.builder.position_at_end(basic_block);

        let compiled_program = compiler.compile_program(&p).unwrap();

        let execution_engine = compiler
            .module
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();
        let main: JitFunction<CompileExpFn> =
            unsafe { execution_engine.get_function("main").unwrap() };
        let result = unsafe { main.call() };

        assert!(result == 3)
    }
}
