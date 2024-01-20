use std::collections::{HashMap, HashSet};

use crate::syntax::ast::{Exp, Stmt, Var};

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Int,
    Bool,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum ExType {
    MonoType(Type),
    TypeVar(String),
}

type TypeEnv = HashMap<Var, Type>;

type TypeConstraint = HashSet<(ExType, ExType)>;

pub fn get_type_vars(tcs: &TypeConstraint) -> HashSet<String> {
    let mut type_vars = HashSet::new();
    for (t1, t2) in tcs {
        match t1 {
            ExType::MonoType(_) => (),
            ExType::TypeVar(s) => {
                type_vars.insert(s.clone());
            }
        }
        match t2 {
            ExType::MonoType(_) => (),
            ExType::TypeVar(s) => {
                type_vars.insert(s.clone());
            }
        }
    }
    type_vars
}

#[derive(Clone)]
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
            psi: Box::new(compose(*psi, s1)),
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
    fn infer(&self, a: T) -> TypeConstraint;
}
