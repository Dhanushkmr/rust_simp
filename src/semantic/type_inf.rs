use std::collections::{HashMap, HashSet};

use crate::syntax::ast::{Const, Exp, Stmt, Var};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Int,
    Bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ExType {
    MonoType(Type),
    TypeVar(String),
}

type TypeEnv = HashMap<Var, Type>;

type TypeConstraint = HashSet<(ExType, ExType)>;

pub fn get_type_vars(tcs: &TypeConstraint) -> HashSet<String> {
    tcs.iter()
        .flat_map(|(left, right)| {
            let mut vars = Vec::new();
            if let ExType::TypeVar(n) = left {
                vars.push(n.clone());
            }
            if let ExType::TypeVar(n) = right {
                vars.push(n.clone())
            }
            vars
        })
        .collect()
}

#[derive(Debug, Clone)]
enum TypeSubst {
    Empty,
    RevComp {
        s: (String, ExType),
        psi: Box<TypeSubst>,
    }, // psi compose s
}

fn single(n: String, t: ExType) -> TypeSubst {
    TypeSubst::RevComp {
        s: (n, t),
        psi: Box::new(TypeSubst::Empty),
    }
}

fn compose(s1: TypeSubst, s2: TypeSubst) -> TypeSubst {
    match s2 {
        TypeSubst::Empty => s1,
        TypeSubst::RevComp { s: (n, t), psi } => TypeSubst::RevComp {
            s: (n, t),
            psi: Box::new(compose(s1, *psi)),
        },
    }
}

trait Substitutable<T> {
    fn apply_subst(&self, s: TypeSubst) -> T;
}

impl Substitutable<ExType> for ExType {
    fn apply_subst(&self, s: TypeSubst) -> ExType {
        match s {
            TypeSubst::Empty => self.clone(),
            TypeSubst::RevComp { s: (n, t), psi } => {
                let outer_ty = match self {
                    ExType::MonoType(_) => self.clone(),
                    ExType::TypeVar(n2) => {
                        if n == *n2 {
                            t
                        } else {
                            ExType::TypeVar(n2.clone())
                        }
                    }
                };
                outer_ty.apply_subst(*psi)
            }
        }
    }
}

impl<A, B> Substitutable<(A, B)> for (A, B)
where
    A: Substitutable<A>,
    B: Substitutable<B>,
{
    fn apply_subst(&self, s: TypeSubst) -> (A, B) {
        (self.0.apply_subst(s.clone()), self.1.apply_subst(s))
    }
}

impl<A> Substitutable<Vec<A>> for Vec<A>
where
    A: Substitutable<A>,
{
    fn apply_subst(&self, s: TypeSubst) -> Vec<A> {
        self.iter().map(|x| x.apply_subst(s.clone())).collect()
    }
}

trait Infer<T> {
    fn infer(&self) -> TypeConstraint;
}

impl<A> Infer<Vec<A>> for Vec<A>
where
    A: Infer<A>,
{
    fn infer(&self) -> TypeConstraint {
        self.iter().fold(HashSet::new(), |k, a| {
            let mut k = k;
            k.extend(a.infer());
            k
        })
    }
}

impl Infer<Stmt> for Stmt {
    fn infer(&self) -> TypeConstraint {
        match self {
            Stmt::Nop => HashSet::new(),
            Stmt::Assign { x, e } => {
                let n = &x.0;
                let alphax = ExType::TypeVar(n.clone());
                let (ex_ty, k) = infer_exp(e.clone());
                {
                    let mut k = k;
                    k.insert((alphax, ex_ty));
                    k
                }
            }
            Stmt::Ret { x: _ } => HashSet::new(),
            Stmt::If { cond, then, el } => {
                let e = infer_exp(cond.clone());
                let kappa_2 = then.infer();
                let kappa_3 = el.infer();
                let mut k = HashSet::new();
                k.insert((e.0, ExType::MonoType(Type::Bool)));
                k.extend(e.1);
                k.extend(kappa_2);
                k.extend(kappa_3);
                k
            }
            Stmt::While { cond, body } => {
                let e = infer_exp(cond.clone());
                let kappa_2 = body.infer();
                let mut k = HashSet::new();
                k.insert((e.0, ExType::MonoType(Type::Bool)));
                k.extend(e.1);
                k.extend(kappa_2);
                k
            }
        }
    }
}

fn infer_exp(e: Exp) -> (ExType, TypeConstraint) {
    match e {
        Exp::ConstExp {
            l: Const::IntConst { v: _ },
        } => (ExType::MonoType(Type::Int), HashSet::new()),
        Exp::ConstExp {
            l: Const::BoolConst { v: _ },
        } => (ExType::MonoType(Type::Bool), HashSet::new()),
        Exp::VarExp { v } => {
            let n = v.0;
            let alphax = ExType::TypeVar(n.clone());
            (alphax, HashSet::new())
        }
        Exp::ParenExp { e } => infer_exp(*e),
        Exp::DEqual { e1, e2 } => {
            let lhs = infer_exp(*e1);
            let rhs = infer_exp(*e2);
            let mut k = HashSet::new();
            k.insert((lhs.0, rhs.0));
            k.extend(lhs.1);
            k.extend(rhs.1);
            (ExType::MonoType(Type::Bool), k)
        }
        Exp::LThan { e1, e2 } => {
            let lhs = infer_exp(*e1);
            let rhs = infer_exp(*e2);
            let mut k = HashSet::new();
            k.insert((lhs.0, rhs.0));
            k.extend(lhs.1);
            k.extend(rhs.1);
            (ExType::MonoType(Type::Bool), k)
        }
        Exp::Plus { e1, e2 } => {
            let lhs = infer_exp(*e1);
            let rhs = infer_exp(*e2);
            let mut k = HashSet::new();
            k.insert((lhs.0, ExType::MonoType(Type::Int)));
            k.insert((rhs.0, ExType::MonoType(Type::Int)));
            k.extend(lhs.1);
            k.extend(rhs.1);
            (ExType::MonoType(Type::Int), k)
        }
        Exp::Minus { e1, e2 } => {
            let lhs = infer_exp(*e1);
            let rhs = infer_exp(*e2);
            let mut k = HashSet::new();
            k.insert((lhs.0, ExType::MonoType(Type::Int)));
            k.insert((rhs.0, ExType::MonoType(Type::Int)));
            k.extend(lhs.1);
            k.extend(rhs.1);
            (ExType::MonoType(Type::Int), k)
        }
        Exp::Mult { e1, e2 } => {
            let lhs = infer_exp(*e1);
            let rhs = infer_exp(*e2);
            let mut k = HashSet::new();
            k.insert((lhs.0, ExType::MonoType(Type::Int)));
            k.insert((rhs.0, ExType::MonoType(Type::Int)));
            k.extend(lhs.1);
            k.extend(rhs.1);
            (ExType::MonoType(Type::Int), k)
        }
    }
}

trait Unifiable<T> {
    fn mgu(&self) -> Result<TypeSubst, String>;
}

impl Unifiable<(ExType, ExType)> for (ExType, ExType) {
    fn mgu(&self) -> Result<TypeSubst, String> {
        match self {
            (ExType::MonoType(t1), ExType::MonoType(t2)) => {
                if t1 == t2 {
                    Ok(TypeSubst::Empty)
                } else {
                    Err("type mismatch".to_string())
                }
            }
            (ExType::TypeVar(n), a) => Ok(TypeSubst::RevComp {
                s: (n.clone(), a.clone()),
                psi: Box::new(TypeSubst::Empty),
            }),
            (a, ExType::TypeVar(n)) => Ok(TypeSubst::RevComp {
                s: (n.clone(), a.clone()),
                psi: Box::new(TypeSubst::Empty),
            }),
        }
    }
}

impl Unifiable<TypeConstraint> for TypeConstraint {
    fn mgu(&self) -> Result<TypeSubst, String> {
        let arr = self.iter().cloned().collect::<Vec<(ExType, ExType)>>();
        arr.mgu()
    }
}

impl Unifiable<Vec<(ExType, ExType)>> for Vec<(ExType, ExType)> {
    fn mgu(&self) -> Result<TypeSubst, String> {
        if self.is_empty() {
            return Ok(TypeSubst::Empty);
        }

        let (head, tail) = self.split_first().unwrap(); // Safe because of the check above
        let head_subst = head.mgu()?;

        // Apply the substitution from unifying the head to the rest of the list
        let tail_subst = tail
            .iter()
            .map(|constraint| constraint.apply_subst(head_subst.clone()))
            .collect::<Vec<(ExType, ExType)>>()
            .mgu()?;

        // Compose the substitutions
        Ok(compose(tail_subst, head_subst))
    }
}

fn ground(varname: String, subst: TypeSubst) -> Result<Type, String> {
    match ExType::TypeVar(varname.clone()).apply_subst(subst.clone()) {
        ExType::MonoType(t) => Ok(t),
        _ => Err(format!(
            "error: type inference failed. {}'s type cannot be grounded {:?}.",
            varname, subst
        )),
    }
}

fn type_inf(s: Vec<Stmt>) -> Result<TypeEnv, String> {
    let type_constraints = s.infer();
    match type_constraints.mgu() {
        Err(err_msg) => Err(err_msg),
        Ok(subst) => {
            let varnames: Vec<String> = get_type_vars(&type_constraints).into_iter().collect();
            let mut acc: TypeEnv = HashMap::new();
            for varname in varnames {
                match ground(varname.clone(), subst.clone()) {
                    Err(err_msg) => return Err(err_msg),
                    Ok(ty) => {
                        acc.insert(Var(varname), ty);
                    }
                }
            }
            Ok(acc)
        }
    }
}

#[cfg(test)]
use pretty_assertions::assert_eq as assert_eq_pretty;
mod tests {
    use super::*;

    #[test]
    fn test_typeinf_type_substitution_a() {
        let psi = single("a".to_string(), ExType::MonoType(Type::Int));
        let expected = ExType::MonoType(Type::Int);
        let result = ExType::TypeVar("a".to_string()).apply_subst(psi);
        assert_eq_pretty!(result, expected);
    }

    #[test]
    fn test_typeinf_type_substitution_b() {
        let psi = single("a".to_string(), ExType::MonoType(Type::Int));
        let expected = ExType::TypeVar("b".to_string());
        let result = ExType::TypeVar("b".to_string()).apply_subst(psi);
        assert_eq_pretty!(result, expected);
    }

    #[test]
    fn test_typeinf_type_substitution_bool() {
        let psi = single("a".to_string(), ExType::MonoType(Type::Int));
        let expected = ExType::MonoType(Type::Bool);
        let result = ExType::MonoType(Type::Bool).apply_subst(psi);
        assert_eq_pretty!(result, expected);
    }

    #[test]
    fn test_typeinf_type_substitution_2_steps() {
        let psi = single("a".to_string(), ExType::MonoType(Type::Int));
        let expected = vec![ExType::MonoType(Type::Bool)];
        let result = vec![ExType::MonoType(Type::Bool)].apply_subst(psi);
        assert_eq_pretty!(result, expected);
    }

    #[test]
    fn test_typeinf_unification_int_bool() {
        let tyconstrs = vec![(ExType::MonoType(Type::Int), ExType::MonoType(Type::Bool))];
        assert!(tyconstrs.mgu().is_err());
    }

    #[test]
    fn test_typeinf_unification_int_a() {
        let tyconstrs = vec![(
            ExType::MonoType(Type::Int),
            ExType::TypeVar("a".to_string()),
        )];
        let expected = ExType::MonoType(Type::Int);
        let result = tyconstrs.mgu().unwrap();
        assert_eq_pretty!(
            ExType::TypeVar("a".to_string()).apply_subst(result),
            expected
        );
    }

    // test typeinf unification {(Int,a), (a, b)} should ground b.
    #[test]
    fn test_typeinf_unification_int_a_a_b() {
        let tyconstrs = vec![
            (
                ExType::MonoType(Type::Int),
                ExType::TypeVar("a".to_string()),
            ),
            (
                ExType::TypeVar("a".to_string()),
                ExType::TypeVar("b".to_string()),
            ),
        ];
        let expected = ExType::MonoType(Type::Int);
        let result = tyconstrs.mgu().unwrap();
        assert_eq_pretty!(
            ExType::TypeVar("b".to_string()).apply_subst(result),
            expected
        );
    }

    #[test]
    fn test_typeinf_unification_a_b_int_int_int_a() {
        let tyconstrs = vec![
            (
                ExType::TypeVar("a".to_string()),
                ExType::TypeVar("b".to_string()),
            ),
            (ExType::MonoType(Type::Int), ExType::MonoType(Type::Int)),
            (
                ExType::MonoType(Type::Int),
                ExType::TypeVar("a".to_string()),
            ),
        ];
        let expected = ExType::MonoType(Type::Int);
        let result = tyconstrs.mgu().unwrap();
        assert_eq_pretty!(
            ExType::TypeVar("b".to_string()).apply_subst(result),
            expected
        );
    }

    // x = input;
    // s = 0;
    // c = 0;
    // while c < x {
    //     s = c + s;
    //     c = c + 1;
    // }
    // return s;
    #[test]
    fn test_typeinf_sum() {
        let input = Var("input".to_string());
        let x = Var("x".to_string());
        let s = Var("s".to_string());
        let c = Var("c".to_string());
        let p = vec![
            Stmt::Assign {
                x: x.clone(),
                e: Exp::VarExp { v: input },
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
        let expected = vec![
            (Var("c".to_string()), Type::Int),
            (Var("x".to_string()), Type::Int),
            (Var("input".to_string()), Type::Int),
            (Var("s".to_string()), Type::Int),
        ]
        .into_iter()
        .collect::<HashMap<Var, Type>>();
        let result = type_inf(p).unwrap();
        assert_eq_pretty!(result, expected);
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
    fn test_typeinf_fib() {
        let input = Var("input".to_string());
        let x = Var("x".to_string());
        let f = Var("f".to_string());
        let s = Var("s".to_string());
        let c = Var("c".to_string());
        let t = Var("t".to_string());
        let p = vec![
            Stmt::Assign {
                x: x.clone(),
                e: Exp::VarExp { v: input },
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
        let expected = vec![
            (Var("f".to_string()), Type::Int),
            (Var("input".to_string()), Type::Int),
            (Var("x".to_string()), Type::Int),
            (Var("s".to_string()), Type::Int),
            (Var("c".to_string()), Type::Int),
            (Var("t".to_string()), Type::Int),
        ]
        .into_iter()
        .collect::<HashMap<Var, Type>>();
        let result = type_inf(p).unwrap();
        assert_eq_pretty!(result, expected);
    }

    /*
    x = input;          // (Œ±_x, Œ±_input)
    y = 0;              // (Œ±_y, int)
    while (y < 3) {     // (Œ±_y, int)
        y = y + 1;      // (Œ±_y, int)
    }
    return y;
    */

    #[test]
    fn test_typeinf_should_fail_in_grounding_x_and_inputs_types() {
        let input = Var("input".to_string());
        let x = Var("x".to_string());
        let y = Var("y".to_string());
        let p = vec![
            Stmt::Assign {
                x: x.clone(),
                e: Exp::VarExp { v: input },
            },
            Stmt::Assign {
                x: y.clone(),
                e: Exp::ConstExp {
                    l: Const::IntConst { v: 0 },
                },
            },
            Stmt::While {
                cond: Exp::LThan {
                    e1: Box::new(Exp::VarExp { v: y.clone() }),
                    e2: Box::new(Exp::ConstExp {
                        l: Const::IntConst { v: 3 },
                    }),
                },
                body: vec![Stmt::Assign {
                    x: y.clone(),
                    e: Exp::Plus {
                        e1: Box::new(Exp::VarExp { v: y.clone() }),
                        e2: Box::new(Exp::ConstExp {
                            l: Const::IntConst { v: 1 },
                        }),
                    },
                }],
            },
            Stmt::Ret { x: y.clone() },
        ];
        let result = type_inf(p);
        assert!(result.is_err())
    }

    /*
    x = input;
    y = 0;
    while (y - x) {
        y = y + 1;
    }
    return y;
    */

    #[test]
    fn test_typeinf_should_fail_in_unifying_int_with_bool() {
        let input = Var("input".to_string());
        let x = Var("x".to_string());
        let y = Var("y".to_string());
        let p = vec![
            Stmt::Assign {
                x: x.clone(),
                e: Exp::VarExp { v: input },
            },
            Stmt::Assign {
                x: y.clone(),
                e: Exp::ConstExp {
                    l: Const::IntConst { v: 0 },
                },
            },
            Stmt::While {
                cond: Exp::Minus {
                    e1: Box::new(Exp::VarExp { v: y.clone() }),
                    e2: Box::new(Exp::VarExp { v: x.clone() }),
                },
                body: vec![Stmt::Assign {
                    x: y.clone(),
                    e: Exp::Plus {
                        e1: Box::new(Exp::VarExp { v: y.clone() }),
                        e2: Box::new(Exp::ConstExp {
                            l: Const::IntConst { v: 1 },
                        }),
                    },
                }],
            },
            Stmt::Ret { x: y.clone() },
        ];
        let result = type_inf(p);
        assert!(result.is_err())
    }
}
