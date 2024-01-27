/*
S ::= X = E; | return X; | nop; | if E { S" } else { S" } | while E { S" }
E ::= E Op E | X | C | (E)
S" ::= S | SS"
Op ::= + | - | * | < | ==
C ::= 1 | 2 | ... | true | false
X ::= a | b | c | d
*/

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Stmt {
    Assign {
        x: Var,
        e: Exp,
    },
    If {
        cond: Exp,
        then: Vec<Stmt>,
        el: Vec<Stmt>,
    },
    Nop,
    While {
        cond: Exp,
        body: Vec<Stmt>,
    },
    Ret {
        x: Var,
    },
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Var(pub String);

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Exp {
    Plus { e1: Box<Exp>, e2: Box<Exp> },
    Minus { e1: Box<Exp>, e2: Box<Exp> },
    Mult { e1: Box<Exp>, e2: Box<Exp> },
    DEqual { e1: Box<Exp>, e2: Box<Exp> },
    LThan { e1: Box<Exp>, e2: Box<Exp> },
    ConstExp { l: Const },
    VarExp { v: Var },
    ParenExp { e: Box<Exp> },
}

#[derive(Debug, PartialEq, Eq)]
pub struct ExpLE {
    pub t: Term,
    pub e_p: ExpLEP,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ExpLEP {
    PlusP { t: Term, e_p: Box<ExpLEP> },
    MinusP { t: Term, e_p: Box<ExpLEP> },
    DEqualP { t: Term, e_p: Box<ExpLEP> },
    LThan { t: Term, e_p: Box<ExpLEP> },
    EpsExpLEP,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Term {
    pub f: Factor,
    pub t_p: TermP,
}

#[derive(Debug, PartialEq, Eq)]
pub enum TermP {
    MultP { f: Factor, t_p: Box<TermP> },
    EpsTermP,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Factor {
    VarExpP { v: Var },
    ConstExpP { l: Const },
    ParenExpP { e: Box<ExpLE> },
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Const {
    IntConst { v: usize },
    BoolConst { v: bool },
}
