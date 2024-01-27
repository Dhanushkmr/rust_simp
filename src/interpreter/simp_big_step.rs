use std::collections::HashMap;

use crate::syntax::ast::{Const, Exp, Stmt, Var};

type Delta = HashMap<Var, Const>;
type ErrMsg = String;

pub fn eval_exp(dlt: &Delta, e: &Exp) -> Result<Const, ErrMsg> {
    match e {
        Exp::ConstExp { l } => Ok(l.clone()),
        Exp::DEqual { e1, e2 } => {
            let c1 = eval_exp(dlt, e1)?;
            let c2 = eval_exp(dlt, e2)?;
            eq_const(&c1, &c2)
        }
        Exp::LThan { e1, e2 } => {
            let c1 = eval_exp(dlt, e1)?;
            let c2 = eval_exp(dlt, e2)?;
            lt_const(&c1, &c2)
        }
        Exp::Minus { e1, e2 } => {
            let c1 = eval_exp(dlt, e1)?;
            let c2 = eval_exp(dlt, e2)?;
            minus_const(&c1, &c2)
        }
        Exp::Mult { e1, e2 } => {
            let c1 = eval_exp(dlt, e1)?;
            let c2 = eval_exp(dlt, e2)?;
            mult_const(&c1, &c2)
        }
        Exp::Plus { e1, e2 } => {
            let c1 = eval_exp(dlt, e1)?;
            let c2 = eval_exp(dlt, e2)?;
            plus_const(&c1, &c2)
        }
        Exp::VarExp { v } => match dlt.get(v) {
            Some(const_value) => Ok(const_value.clone()),
            None => Err(format!("Variable {} not found in the environment.", v.0)),
        },
        Exp::ParenExp { e } => eval_exp(dlt, e),
    }
}

pub fn eval_many(dlt: &Delta, ss: &Vec<Stmt>) -> Result<Delta, ErrMsg> {
    let mut dlt_prime = dlt.clone();
    for s in ss {
        dlt_prime = eval_one(&dlt_prime, s)?;
    }
    Ok(dlt_prime)
}

pub fn eval_one(dlt: &Delta, s: &Stmt) -> Result<Delta, ErrMsg> {
    match s {
        Stmt::Nop => Ok(dlt.clone()),
        Stmt::Assign { x, e } => {
            let c = eval_exp(dlt, e)?;
            let mut dlt_prime = dlt.clone();
            dlt_prime.insert(x.clone(), c);
            Ok(dlt_prime)
        }
        Stmt::If { cond, then, el } => {
            let c = eval_exp(dlt, cond)?;
            match c {
                Const::IntConst { v: _ } => {
                    Err("int expression found in the if condition position.".to_string())
                }
                Const::BoolConst { v: _ } => {
                    if c == (Const::BoolConst { v: true }) {
                        eval_many(dlt, then)
                    } else {
                        eval_many(dlt, el)
                    }
                }
            }
        }
        Stmt::Ret { x: _ } => Ok(dlt.clone()),
        Stmt::While { cond, body } => {
            let c = eval_exp(dlt, cond)?;
            match c {
                Const::IntConst { v: _ } => {
                    Err("int expression found in the while condition position.".to_string())
                }
                Const::BoolConst { v: true } => {
                    let dlt_body = eval_many(dlt, body)?;
                    eval_one(&dlt_body, s)
                }
                Const::BoolConst { v: false } => Ok(dlt.clone()),
            }
        }
    }
}

fn eq_const(c1: &Const, c2: &Const) -> Result<Const, ErrMsg> {
    match (c1, c2) {
        (Const::IntConst { v: v1 }, Const::IntConst { v: v2 }) => {
            Ok(Const::BoolConst { v: v1 == v2 })
        }
        (Const::BoolConst { v: v1 }, Const::BoolConst { v: v2 }) => {
            Ok(Const::BoolConst { v: v1 == v2 })
        }
        (_, _) => Err("different types of values are compared using ==".to_string()),
    }
}

fn lt_const(c1: &Const, c2: &Const) -> Result<Const, ErrMsg> {
    match (c1, c2) {
        (Const::IntConst { v: v1 }, Const::IntConst { v: v2 }) => {
            Ok(Const::BoolConst { v: v1 < v2 })
        }
        (_, _) => Err("non int type of values are compared using <".to_string()),
    }
}

fn minus_const(c1: &Const, c2: &Const) -> Result<Const, ErrMsg> {
    match (c1, c2) {
        (Const::IntConst { v: v1 }, Const::IntConst { v: v2 }) => {
            Ok(Const::IntConst { v: v1 - v2 })
        }
        (_, _) => Err("non int type of values are used with -".to_string()),
    }
}

fn mult_const(c1: &Const, c2: &Const) -> Result<Const, ErrMsg> {
    match (c1, c2) {
        (Const::IntConst { v: v1 }, Const::IntConst { v: v2 }) => {
            Ok(Const::IntConst { v: v1 * v2 })
        }
        (_, _) => Err("non int type of values are used with *".to_string()),
    }
}

fn plus_const(c1: &Const, c2: &Const) -> Result<Const, ErrMsg> {
    match (c1, c2) {
        (Const::IntConst { v: v1 }, Const::IntConst { v: v2 }) => {
            Ok(Const::IntConst { v: v1 + v2 })
        }
        (_, _) => Err("non int type of values are used with +".to_string()),
    }
}

pub fn interpret(p: &Vec<Stmt>, input: usize) -> Result<Const, ErrMsg> {
    let mut dlt: Delta = HashMap::new();
    dlt.insert(Var("input".to_string()), Const::IntConst { v: input });
    let last_ret_var = get_last_ret_var(p)?;
    match last_ret_var {
        Var(name) => {
            let dlt_prime = eval_many(&dlt, p)?;
            match dlt_prime.get(&Var(name.clone())) {
                Some(v) => Ok(v.clone()),
                None => Err(format!("undefined variable {}.", name)),
            }
        }
    }
}

fn get_last_ret_var(p: &Vec<Stmt>) -> Result<Var, ErrMsg> {
    match p.last() {
        None => Err("error: the program is empty".to_string()),
        Some(Stmt::Ret { x }) => Ok(x.clone()),
        Some(_) => {
            Err("error: the last statement of the SIMP program is not a return.".to_string())
        }
    }
}

use pretty_assertions::assert_eq as assert_eq_pretty;
#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_simp_eval_exp_with_c_plus_s() {
        let s = Var("s".to_string());
        let c = Var("c".to_string());
        let mut delta: Delta = HashMap::new();
        delta.insert(s.clone(), Const::IntConst { v: 3 });
        delta.insert(c.clone(), Const::IntConst { v: 1 });
        let exp1: Exp = Exp::Plus {
            e1: Box::new(Exp::VarExp { v: c.clone() }),
            e2: Box::new(Exp::VarExp { v: s.clone() }),
        };
        let expected = Const::IntConst { v: 4 };
        match eval_exp(&delta, &exp1) {
            Err(error) => {
                assert_eq_pretty!(error, "".to_string());
            }
            Ok(result) => {
                assert_eq_pretty!(result, expected);
            }
        }
    }

    #[test]
    fn test_simp_eval_exp_with_c_lthan() {
        let s = Var("s".to_string());
        let c = Var("c".to_string());
        let mut delta: Delta = HashMap::new();
        delta.insert(s.clone(), Const::IntConst { v: 3 });
        delta.insert(c.clone(), Const::IntConst { v: 1 });
        let exp1: Exp = Exp::DEqual {
            e1: Box::new(Exp::VarExp { v: c.clone() }),
            e2: Box::new(Exp::VarExp { v: s.clone() }),
        };
        let expected = Const::BoolConst { v: false };
        match eval_exp(&delta, &exp1) {
            Err(error) => {
                assert_eq_pretty!(error, "".to_string());
            }
            Ok(result) => {
                assert_eq_pretty!(result, expected);
            }
        }
    }

    /*
    x = input;
    s = 0;
    c = 0;
    while c < x {
        s = c + s;
        c = c + 1;
    }
    return s;
    */

    #[test]
    fn test_simp_interpret_with_sum_3() {
        let input = Var("input".to_string());
        let x = Var("x".to_string());
        let s = Var("s".to_string());
        let c = Var("c".to_string());
        let p = vec![
            Stmt::Assign {
                x: x.clone(),
                e: Exp::VarExp { v: input.clone() },
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
        let expected = Const::IntConst { v: 3 };
        match interpret(&p, 3) {
            Err(error) => {
                assert_eq_pretty!(error, "".to_string());
            }
            Ok(result) => {
                assert_eq_pretty!(result, expected);
            }
        }
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
        let input = Var("input".to_string());
        let x = Var("x".to_string());
        let f = Var("f".to_string());
        let s = Var("s".to_string());
        let c = Var("c".to_string());
        let t = Var("t".to_string());
        let p = vec![
            Stmt::Assign {
                x: x.clone(),
                e: Exp::VarExp { v: input.clone() },
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
        let expected = Const::IntConst { v: 5 };
        match interpret(&p, 4) {
            Err(error) => {
                assert_eq_pretty!(error, "".to_string());
            }
            Ok(result) => {
                assert_eq_pretty!(result, expected);
            }
        }
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
    //  sum((0 to 10).map(fib(_))

    #[test]
    fn test_simp_interpret_with_sum_4() {
        let input = Var("input".to_string());
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
                e: Exp::VarExp { v: input.clone() },
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
        let expected = Const::IntConst { v: 7 };
        match interpret(&p, 4) {
            Err(error) => {
                assert_eq_pretty!(error, "".to_string());
            }
            Ok(result) => {
                assert_eq_pretty!(result, expected);
            }
        }
    }
}
