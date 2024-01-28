use inkwell::module::Module;
use inkwell::values::{BasicValueEnum, PointerValue};
use inkwell::IntPredicate;
use inkwell::{builder::BuilderError, context::Context};
use std::collections::HashMap;

use crate::syntax::ast::{Const, Exp, Stmt, Var};

pub struct Compiler<'ctx> {
    context: &'ctx Context,
    builder: inkwell::builder::Builder<'ctx>,
    variables: HashMap<String, PointerValue<'ctx>>,
    pub module: Module<'ctx>,
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

    pub fn compile_exp(&mut self, exp: Exp) -> Result<BasicValueEnum<'ctx>, BuilderError> {
        match exp {
            Exp::Plus { e1, e2 } => {
                let e1_value = self.compile_exp(*e1)?;
                let e2_value = self.compile_exp(*e2)?;
                match (e1_value, e2_value) {
                    (BasicValueEnum::IntValue(e1_value), BasicValueEnum::IntValue(e2_value)) => {
                        Ok(BasicValueEnum::IntValue(
                            self.builder.build_int_add(e1_value, e2_value, "add")?,
                        ))
                    }
                    _ => Err(BuilderError::ValueTypeMismatch("expected int values"))?,
                }
            }
            Exp::Minus { e1, e2 } => {
                let e1_value = self.compile_exp(*e1)?;
                let e2_value = self.compile_exp(*e2)?;
                match (e1_value, e2_value) {
                    (BasicValueEnum::IntValue(e1_value), BasicValueEnum::IntValue(e2_value)) => {
                        Ok(BasicValueEnum::IntValue(
                            self.builder.build_int_sub(e1_value, e2_value, "sub")?,
                        ))
                    }
                    _ => Err(BuilderError::ValueTypeMismatch("expected int values"))?,
                }
            }
            Exp::Mult { e1, e2 } => {
                let e1_value = self.compile_exp(*e1)?;
                let e2_value = self.compile_exp(*e2)?;
                match (e1_value, e2_value) {
                    (BasicValueEnum::IntValue(e1_value), BasicValueEnum::IntValue(e2_value)) => {
                        Ok(BasicValueEnum::IntValue(
                            self.builder.build_int_mul(e1_value, e2_value, "mul")?,
                        ))
                    }
                    _ => Err(BuilderError::ValueTypeMismatch("expected int values"))?,
                }
            }
            Exp::DEqual { e1, e2 } => {
                let e1_value = self.compile_exp(*e1)?;
                let e2_value = self.compile_exp(*e2)?;
                match (e1_value, e2_value) {
                    (BasicValueEnum::IntValue(e1_value), BasicValueEnum::IntValue(e2_value)) => {
                        Ok(BasicValueEnum::IntValue(self.builder.build_int_compare(
                            IntPredicate::EQ,
                            e1_value,
                            e2_value,
                            "eq",
                        )?))
                    }
                    _ => Err(BuilderError::ValueTypeMismatch("expected int values"))?,
                }
            }
            Exp::LThan { e1, e2 } => {
                let e1_value = self.compile_exp(*e1)?;
                let e2_value = self.compile_exp(*e2)?;
                match (e1_value, e2_value) {
                    (BasicValueEnum::IntValue(e1_value), BasicValueEnum::IntValue(e2_value)) => {
                        Ok(BasicValueEnum::IntValue(self.builder.build_int_compare(
                            IntPredicate::SLT,
                            e1_value,
                            e2_value,
                            "slt",
                        )?))
                    }
                    _ => Err(BuilderError::ValueTypeMismatch("expected int values"))?,
                }
            }
            Exp::ConstExp { l } => match l {
                Const::IntConst { v } => Ok(BasicValueEnum::IntValue(
                    self.context.i32_type().const_int(v as u64, false),
                )),
                Const::BoolConst { v } => Ok(BasicValueEnum::IntValue(
                    self.context.bool_type().const_int(v as u64, false),
                )),
            },
            Exp::VarExp { v } => {
                let variable = self
                    .variables
                    .get(&v.0)
                    .ok_or("variable not found")
                    .unwrap();
                Ok(self
                    .builder
                    .build_load(self.context.i32_type(), *variable, &v.0)?)
            }
            Exp::ParenExp { e } => self.compile_exp(*e),
        }
    }

    pub fn compile_stmt(&mut self, stmt: Stmt) -> Result<(), Box<dyn std::error::Error>> {
        println!("stmt: {:?}", stmt);
        match stmt {
            Stmt::Assign { x, e } => {
                let value = self.compile_exp(e)?;
                let variable = self.variables.entry(x.0).or_insert_with(|| {
                    let alloca = self.builder.build_alloca(value.get_type(), "var").unwrap();
                    self.builder.build_store(alloca, value).unwrap();
                    alloca
                });
                self.builder.build_store(*variable, value).unwrap();
                Ok(())
            }
            Stmt::If { cond, then, el } => {
                let cond_value = self.compile_exp(cond)?;
                let cond_value_int = match cond_value {
                    BasicValueEnum::IntValue(cond_value) => cond_value,
                    _ => Err(BuilderError::ValueTypeMismatch("expected int values"))?,
                };

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
                    .build_conditional_branch(cond_value_int, then_block, else_block)?;

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

                // self.builder.position_at_end(then_block);

                // for stmt in then {
                //     self.compile_stmt(stmt)?;
                // }
                // println!("then branch: {:?}", &self.variables);
                // self.builder.build_unconditional_branch(cont_block)?;

                // self.builder.position_at_end(else_block);

                // for stmt in el {
                //     self.compile_stmt(stmt)?;
                // }
                // println!("else branch: {:?}", &self.variables);
                // self.builder.build_unconditional_branch(cont_block)?;
                // println!("cont block: {:?}", &self.variables);
                // self.builder.position_at_end(cont_block);
                // Ok(())
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
                let cond_value_int = match cond_value {
                    BasicValueEnum::IntValue(cond_value) => cond_value,
                    _ => Err(BuilderError::ValueTypeMismatch("expected int values"))?,
                };
                self.builder
                    .build_conditional_branch(cond_value_int, body_block, end_block)?;

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
                let return_pointer = *self.variables.get(&x.0).unwrap();
                //look up the variable in memory and return it
                let return_value =
                    self.builder
                        .build_load(self.context.i32_type(), return_pointer, &x.0)?;

                self.builder.build_return(Some(&return_value))?;
                Ok(())
            }
        }
    }

    pub fn compile_block(&mut self, block: &[Stmt]) -> Result<(), Box<dyn std::error::Error>> {
        for stmt in block {
            self.compile_stmt(stmt.clone())?;
        }
        Ok(())
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
        let return_pointer = *compiler.variables.get("x").unwrap();
        //look up the variable in memory and return it
        let return_value = compiler
            .builder
            .build_load(compiler.context.i32_type(), return_pointer, "x")
            .unwrap();
        compiler.builder.build_return(Some(&return_value)).unwrap();

        let execution_engine = compiler
            .module
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();
        println!("{}", compiler.module.print_to_string().to_string());
        let main: JitFunction<CompileExpFn> =
            unsafe { execution_engine.get_function("main").unwrap() };
        let result = unsafe { main.call() };

        assert_eq!(result, 8);
    }

    #[test]
    fn test_compile_2_assign_1_ret() {
        let context = Context::create();
        let mut compiler = Compiler::new(&context);

        let stmts = vec![
            Stmt::Assign {
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
            },
            Stmt::Assign {
                x: Var("y".to_string()),
                e: Exp::ConstExp {
                    l: Const::IntConst { v: 5 },
                },
            },
            Stmt::Ret {
                x: Var("x".to_string()),
            },
        ];

        compiler.compile_program(&stmts).unwrap();
        println!("{}", compiler.module.print_to_string().to_string());

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
    fn test_compile_1_assign_1_if_1_ret() {
        let context = Context::create();
        let mut compiler = Compiler::new(&context);

        let stmts = vec![
            Stmt::Assign {
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
            },
            Stmt::Assign {
                x: Var("x".to_string()),
                e: Exp::ConstExp {
                    l: Const::IntConst { v: 10 },
                },
            },
            Stmt::If {
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
            },
            Stmt::Ret {
                x: Var("x".to_string()),
            },
        ];

        compiler.compile_program(&stmts).unwrap();
        println!("{}", compiler.module.print_to_string().to_string());

        let execution_engine = compiler
            .module
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();

        let main: JitFunction<CompileExpFn> =
            unsafe { execution_engine.get_function("main").unwrap() };
        let result = unsafe { main.call() };

        assert_eq!(result, 9);
    }

    /*
    x = input;
    f = 0;
    s = 1;
    c = 0;
    t = 0;
    while c < x {
        t = f;
        f = s;
        s = t + f;
        c = c + 1;
    }
    return s;
    */

    #[test]
    fn test_fib_4() {
        let context = Context::create();
        let mut compiler = Compiler::new(&context);

        let x = Var("x".to_string());
        let f = Var("f".to_string());
        let s = Var("s".to_string());
        let c = Var("c".to_string());
        let t = Var("t".to_string());
        let p = vec![
            Stmt::Assign {
                x: x.clone(),
                e: Exp::ConstExp {
                    l: Const::IntConst { v: 4 },
                },
            },
            Stmt::Assign {
                x: f.clone(),
                e: Exp::ConstExp {
                    l: Const::IntConst { v: 0 },
                },
            },
            Stmt::Assign {
                x: s.clone(),
                e: Exp::ConstExp {
                    l: Const::IntConst { v: 1 },
                },
            },
            Stmt::Assign {
                x: c.clone(),
                e: Exp::ConstExp {
                    l: Const::IntConst { v: 0 },
                },
            },
            Stmt::Assign {
                x: t.clone(),
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
                        x: t.clone(),
                        e: Exp::VarExp { v: f.clone() },
                    },
                    Stmt::Assign {
                        x: f.clone(),
                        e: Exp::VarExp { v: s.clone() },
                    },
                    Stmt::Assign {
                        x: s.clone(),
                        e: Exp::Plus {
                            e1: Box::new(Exp::VarExp { v: t.clone() }),
                            e2: Box::new(Exp::VarExp { v: f.clone() }),
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
        compiler.compile_program(&p).unwrap();
        println!("{}", compiler.module.print_to_string().to_string());

        let execution_engine = compiler
            .module
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();

        let main: JitFunction<CompileExpFn> =
            unsafe { execution_engine.get_function("main").unwrap() };
        let result = unsafe { main.call() };

        assert_eq!(result, 5);
    }

    /*
    x = input;
    r = 0;
    i = 0;
    while i < x {
        f = 0;
        s = 1;
        j = 0;
        t = 0;
        while j < i {
            t = f;
            f = s;
            s = t + f;
            j = j + 1;
        }
        r = r + s;
        i = i + 1;
    }
    return r;
    */

    #[test]
    fn test_sum_fib_4() {
        let context = Context::create();
        let mut compiler = Compiler::new(&context);

        let x = Var("x".to_string());
        let r = Var("r".to_string());
        let i = Var("i".to_string());
        let f = Var("f".to_string());
        let s = Var("s".to_string());
        let j = Var("j".to_string());
        let t = Var("t".to_string());
        let p = vec![
            Stmt::Assign {
                x: x.clone(),
                e: Exp::ConstExp {
                    l: Const::IntConst { v: 4 },
                },
            },
            Stmt::Assign {
                x: r.clone(),
                e: Exp::ConstExp {
                    l: Const::IntConst { v: 0 },
                },
            },
            Stmt::Assign {
                x: i.clone(),
                e: Exp::ConstExp {
                    l: Const::IntConst { v: 0 },
                },
            },
            Stmt::While {
                cond: Exp::LThan {
                    e1: Box::new(Exp::VarExp { v: i.clone() }),
                    e2: Box::new(Exp::VarExp { v: x.clone() }),
                },
                body: vec![
                    Stmt::Assign {
                        x: f.clone(),
                        e: Exp::ConstExp {
                            l: Const::IntConst { v: 0 },
                        },
                    },
                    Stmt::Assign {
                        x: s.clone(),
                        e: Exp::ConstExp {
                            l: Const::IntConst { v: 1 },
                        },
                    },
                    Stmt::Assign {
                        x: j.clone(),
                        e: Exp::ConstExp {
                            l: Const::IntConst { v: 0 },
                        },
                    },
                    Stmt::Assign {
                        x: t.clone(),
                        e: Exp::ConstExp {
                            l: Const::IntConst { v: 0 },
                        },
                    },
                    Stmt::While {
                        cond: Exp::LThan {
                            e1: Box::new(Exp::VarExp { v: j.clone() }),
                            e2: Box::new(Exp::VarExp { v: i.clone() }),
                        },
                        body: vec![
                            Stmt::Assign {
                                x: t.clone(),
                                e: Exp::VarExp { v: f.clone() },
                            },
                            Stmt::Assign {
                                x: f.clone(),
                                e: Exp::VarExp { v: s.clone() },
                            },
                            Stmt::Assign {
                                x: s.clone(),
                                e: Exp::Plus {
                                    e1: Box::new(Exp::VarExp { v: t.clone() }),
                                    e2: Box::new(Exp::VarExp { v: f.clone() }),
                                },
                            },
                            Stmt::Assign {
                                x: j.clone(),
                                e: Exp::Plus {
                                    e1: Box::new(Exp::VarExp { v: j.clone() }),
                                    e2: Box::new(Exp::ConstExp {
                                        l: Const::IntConst { v: 1 },
                                    }),
                                },
                            },
                        ],
                    },
                    Stmt::Assign {
                        x: r.clone(),
                        e: Exp::Plus {
                            e1: Box::new(Exp::VarExp { v: r.clone() }),
                            e2: Box::new(Exp::VarExp { v: s.clone() }),
                        },
                    },
                    Stmt::Assign {
                        x: i.clone(),
                        e: Exp::Plus {
                            e1: Box::new(Exp::VarExp { v: i.clone() }),
                            e2: Box::new(Exp::ConstExp {
                                l: Const::IntConst { v: 1 },
                            }),
                        },
                    },
                ],
            },
            Stmt::Ret { x: r.clone() },
        ];
        compiler.compile_program(&p).unwrap();
        println!("{}", compiler.module.print_to_string().to_string());

        let execution_engine = compiler
            .module
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();

        let main: JitFunction<CompileExpFn> =
            unsafe { execution_engine.get_function("main").unwrap() };
        let result = unsafe { main.call() };

        assert_eq!(result, 7);
    }
}
