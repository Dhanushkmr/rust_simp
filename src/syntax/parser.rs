use super::ast::*;
use super::lexer::{LToken, Token};
use nom::combinator::opt;
use nom::{
    branch::alt,
    error::{ErrorKind, ParseError},
    multi::many0,
    Err, IResult,
};
use std::mem::discriminant;

fn consume_token(expected: LToken) -> impl Fn(&[Token]) -> IResult<&[Token], &Token> {
    move |input: &[Token]| {
        if input.is_empty() {
            Err(Err::Error(ParseError::from_error_kind(
                input,
                ErrorKind::Eof,
            )))
        } else if discriminant(&input[0].type_) == discriminant(&expected) {
            Ok((&input[1..], &input[0]))
        } else {
            Err(Err::Error(ParseError::from_error_kind(
                input,
                ErrorKind::Tag,
            )))
        }
    }
}

pub fn parse_stmts(input: &[Token]) -> IResult<&[Token], Vec<Stmt>> {
    let (input, stmts) = many0(parse_one)(input)?;
    Ok((input, stmts))
}

fn parse_one(input: &[Token]) -> IResult<&[Token], Stmt> {
    let (input, _) = parse_spaces(input)?;
    let (input, stmt) = parse_stmt(input)?;
    let (input, _) = parse_spaces(input)?;
    Ok((input, stmt))
}

fn parse_space(input: &[Token]) -> IResult<&[Token], ()> {
    let (input, _) = alt((
        consume_token(LToken::WhiteSpace { c: ' ' }),
        consume_token(LToken::WhiteSpace { c: '\n' }),
    ))(input)?;
    Ok((input, ()))
}

fn parse_spaces(input: &[Token]) -> IResult<&[Token], ()> {
    let (input, _) = many0(parse_space)(input)?;
    Ok((input, ()))
}

fn parse_stmt(input: &[Token]) -> IResult<&[Token], Stmt> {
    alt((parse_assign, parse_if, parse_nop, parse_while, parse_ret))(input)
}

fn parse_assign(input: &[Token]) -> IResult<&[Token], Stmt> {
    let (input, token) = consume_token(LToken::IdTok { v: "".to_string() })(input)?;
    let var_name = match &token.type_ {
        LToken::IdTok { v } => v.clone(),
        _ => unreachable!(), // We checked the token type in consume_token_type, so this should never happen
    };
    let (input, _) = parse_spaces(input)?;
    let (input, _) = consume_token(LToken::EqSign)(input)?;
    let (input, _) = parse_spaces(input)?;
    let (input, exp) = parse_exp(input)?;
    let (input, _) = consume_token(LToken::SemiColon)(input)?;
    Ok((
        input,
        Stmt::Assign {
            x: Var(var_name),
            e: exp,
        },
    ))
}

fn parse_if(input: &[Token]) -> IResult<&[Token], Stmt> {
    let (input, _) = consume_token(LToken::IfKW)(input)?;
    let (input, _) = parse_spaces(input)?;
    let (input, exp) = parse_exp(input)?;
    let (input, _) = parse_spaces(input)?;
    let (input, _) = consume_token(LToken::LBrace)(input)?;
    let (input, _) = parse_spaces(input)?;
    let (input, then) = parse_stmts(input)?;
    let (input, _) = parse_spaces(input)?;
    let (input, _) = consume_token(LToken::RBrace)(input)?;
    let (input, _) = parse_spaces(input)?;
    let (input, _) = consume_token(LToken::ElseKW)(input)?;
    let (input, _) = parse_spaces(input)?;
    let (input, _) = consume_token(LToken::LBrace)(input)?;
    let (input, _) = parse_spaces(input)?;
    let (input, el) = parse_stmts(input)?;
    let (input, _) = parse_spaces(input)?;
    let (input, _) = consume_token(LToken::RBrace)(input)?;
    Ok((
        input,
        Stmt::If {
            cond: exp,
            then,
            el,
        },
    ))
}

fn parse_nop(input: &[Token]) -> IResult<&[Token], Stmt> {
    let (input, _) = consume_token(LToken::NopKW)(input)?;
    let (input, _) = consume_token(LToken::SemiColon)(input)?;
    Ok((input, Stmt::Nop))
}

fn parse_while(input: &[Token]) -> IResult<&[Token], Stmt> {
    let (input, _) = consume_token(LToken::WhileKW)(input)?;
    let (input, _) = parse_spaces(input)?;
    let (input, exp) = parse_exp(input)?;
    let (input, _) = parse_spaces(input)?;
    let (input, _) = consume_token(LToken::LBrace)(input)?;
    let (input, _) = parse_spaces(input)?;
    let (input, body) = parse_stmts(input)?;
    let (input, _) = parse_spaces(input)?;
    let (input, _) = consume_token(LToken::RBrace)(input)?;
    Ok((input, Stmt::While { cond: exp, body }))
}

fn parse_ret(input: &[Token]) -> IResult<&[Token], Stmt> {
    let (input, _) = consume_token(LToken::RetKW)(input)?;
    let (input, _) = parse_spaces(input)?;
    let (input, token) = consume_token(LToken::IdTok { v: "".to_string() })(input)?;
    let var_name = match &token.type_ {
        LToken::IdTok { v } => v.clone(),
        _ => unreachable!(), // We checked the token type in consume_token_type, so this should never happen
    };
    let (input, _) = consume_token(LToken::SemiColon)(input)?;
    Ok((input, Stmt::Ret { x: Var(var_name) }))
}

fn parse_exp(input: &[Token]) -> IResult<&[Token], Exp> {
    let (input, e_p) = parse_exp_left_rec(input)?;
    Ok((input, from_exp_le(e_p)))
}

fn parse_exp_left_rec(input: &[Token]) -> IResult<&[Token], ExpLE> {
    let (input, t) = parse_term(input)?;
    let (input, e_p) = parse_exp_p(input)?;
    Ok((input, ExpLE { t, e_p }))
}

fn parse_term(input: &[Token]) -> IResult<&[Token], Term> {
    let (input, f) = parse_factor(input)?;
    let (input, t_p) = parse_term_p(input)?;
    Ok((input, Term { f, t_p }))
}

fn parse_term_p(input: &[Token]) -> IResult<&[Token], TermP> {
    let (input, omt) = opt(parse_multiplication)(input)?;
    let term_p = match omt {
        Some(mult) => mult,
        None => TermP::EpsTermP,
    };
    Ok((input, term_p))
}

fn parse_factor(input: &[Token]) -> IResult<&[Token], Factor> {
    alt((parse_var_exp_p, parse_const_exp, parse_paren_exp_left_rec))(input)
}

fn parse_const_exp(input: &[Token]) -> IResult<&[Token], Factor> {
    let (input, token) = parse_const(input)?;
    match token {
        Const::BoolConst { v: _ } => Ok((input, Factor::ConstExpP { l: token })),
        Const::IntConst { v: _ } => Ok((input, Factor::ConstExpP { l: token })),
    }
}

fn parse_var_exp_p(input: &[Token]) -> IResult<&[Token], Factor> {
    let (input, token) = consume_token(LToken::IdTok { v: "".to_string() })(input)?;
    let var_name = match &token.type_ {
        LToken::IdTok { v } => v.clone(),
        _ => unreachable!(), // We checked the token type in consume_token_type, so this should never happen
    };
    Ok((input, Factor::VarExpP { v: Var(var_name) }))
}

fn parse_paren_exp_left_rec(input: &[Token]) -> IResult<&[Token], Factor> {
    let (input, _) = consume_token(LToken::LParen)(input)?;
    let (input, e) = parse_exp_left_rec(input)?;
    let (input, _) = consume_token(LToken::RParen)(input)?;
    Ok((input, Factor::ParenExpP { e: Box::new(e) }))
}

fn parse_exp_p(input: &[Token]) -> IResult<&[Token], ExpLEP> {
    let (input, oep) = opt(alt((
        parse_addition,
        parse_subtraction,
        parse_dequal,
        parse_lthan,
    )))(input)?;
    let exp_p = match oep {
        Some(ep) => ep,
        None => ExpLEP::EpsExpLEP,
    };
    Ok((input, exp_p))
}

fn parse_addition(input: &[Token]) -> IResult<&[Token], ExpLEP> {
    let (input, _) = parse_spaces(input)?;
    let (input, _) = consume_token(LToken::PlusSign)(input)?;
    let (input, _) = parse_spaces(input)?;
    let (input, t) = parse_term(input)?;
    let (input, e_p) = parse_exp_p(input)?;
    Ok((
        input,
        ExpLEP::PlusP {
            t,
            e_p: Box::new(e_p),
        },
    ))
}

fn parse_subtraction(input: &[Token]) -> IResult<&[Token], ExpLEP> {
    let (input, _) = parse_spaces(input)?;
    let (input, _) = consume_token(LToken::MinusSign)(input)?;
    let (input, _) = parse_spaces(input)?;
    let (input, t) = parse_term(input)?;
    let (input, e_p) = parse_exp_p(input)?;
    Ok((
        input,
        ExpLEP::MinusP {
            t,
            e_p: Box::new(e_p),
        },
    ))
}

fn parse_dequal(input: &[Token]) -> IResult<&[Token], ExpLEP> {
    let (input, _) = parse_spaces(input)?;
    let (input, _) = consume_token(LToken::DEqSign)(input)?;
    let (input, _) = parse_spaces(input)?;
    let (input, t) = parse_term(input)?;
    let (input, e_p) = parse_exp_p(input)?;
    Ok((
        input,
        ExpLEP::DEqualP {
            t,
            e_p: Box::new(e_p),
        },
    ))
}

fn parse_lthan(input: &[Token]) -> IResult<&[Token], ExpLEP> {
    let (input, _) = parse_spaces(input)?;
    let (input, _) = consume_token(LToken::LThanSign)(input)?;
    let (input, _) = parse_spaces(input)?;
    let (input, t) = parse_term(input)?;
    let (input, e_p) = parse_exp_p(input)?;
    Ok((
        input,
        ExpLEP::LThan {
            t,
            e_p: Box::new(e_p),
        },
    ))
}

fn parse_multiplication(input: &[Token]) -> IResult<&[Token], TermP> {
    let (input, _) = parse_spaces(input)?;
    let (input, _) = consume_token(LToken::AsteriskSign)(input)?;
    let (input, _) = parse_spaces(input)?;
    let (input, t) = parse_factor(input)?;
    let (input, t_p) = parse_term_p(input)?;
    Ok((
        input,
        TermP::MultP {
            f: t,
            t_p: Box::new(t_p),
        },
    ))
}

fn parse_const(input: &[Token]) -> IResult<&[Token], Const> {
    alt((parse_true, parse_false, parse_int))(input)
}

fn parse_true(input: &[Token]) -> IResult<&[Token], Const> {
    let (input, token) = consume_token(LToken::TrueKW)(input)?;
    match &token.type_ {
        LToken::TrueKW => Ok((input, Const::BoolConst { v: true })),
        _ => unreachable!(), // We checked the token type in consume_token_type, so this should never happen
    }
}

fn parse_false(input: &[Token]) -> IResult<&[Token], Const> {
    let (input, token) = consume_token(LToken::FalseKW)(input)?;
    match &token.type_ {
        LToken::FalseKW => Ok((input, Const::BoolConst { v: false })),
        _ => unreachable!(), // We checked the token type in consume_token_type, so this should never happen
    }
}

fn parse_int(input: &[Token]) -> IResult<&[Token], Const> {
    let (input, token) = consume_token(LToken::IntTok { v: 0 })(input)?;
    match &token.type_ {
        LToken::IntTok { v } => Ok((input, Const::IntConst { v: *v })),
        _ => unreachable!(), // We checked the token type in consume_token_type, so this should never happen
    }
}

fn from_exp_le(exp_le: ExpLE) -> Exp {
    let ExpLE { t, e_p } = exp_le;
    from_exp_lep(from_term(t), e_p)
}

fn from_exp_lep(e1: Exp, ep1: ExpLEP) -> Exp {
    match ep1 {
        ExpLEP::PlusP { t, e_p } => from_exp_lep(
            Exp::Plus {
                e1: Box::new(e1),
                e2: Box::new(from_term(t)),
            },
            *e_p,
        ),
        ExpLEP::MinusP { t, e_p } => from_exp_lep(
            Exp::Minus {
                e1: Box::new(e1),
                e2: Box::new(from_term(t)),
            },
            *e_p,
        ),
        ExpLEP::DEqualP { t, e_p } => from_exp_lep(
            Exp::DEqual {
                e1: Box::new(e1),
                e2: Box::new(from_term(t)),
            },
            *e_p,
        ),
        ExpLEP::LThan { t, e_p } => from_exp_lep(
            Exp::LThan {
                e1: Box::new(e1),
                e2: Box::new(from_term(t)),
            },
            *e_p,
        ),
        ExpLEP::EpsExpLEP => e1,
    }
}

fn from_term(t: Term) -> Exp {
    let Term { f, t_p } = t;
    from_term_p(from_factor(f), t_p)
}

fn from_term_p(e1: Exp, t_p1: TermP) -> Exp {
    match t_p1 {
        TermP::EpsTermP => e1,
        TermP::MultP { f, t_p } => {
            let a2 = Exp::Mult {
                e1: Box::new(e1),
                e2: Box::new(from_factor(f)),
            };
            from_term_p(a2, *t_p)
        }
    }
}

fn from_factor(f: Factor) -> Exp {
    match f {
        Factor::VarExpP { v } => Exp::VarExp { v },
        Factor::ConstExpP { l } => Exp::ConstExp { l },
        Factor::ParenExpP { e } => Exp::ParenExp {
            e: Box::new(from_exp_le(*e)),
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::lexer::{LToken, SrcLoc, Token};

    #[test]
    fn test_parse_assign() {
        let input = vec![
            Token {
                type_: LToken::IdTok { v: "a".to_string() },
                src_loc: SrcLoc { line: 1, col: 1 },
            },
            Token {
                type_: LToken::WhiteSpace { c: ' ' },
                src_loc: SrcLoc { line: 1, col: 2 },
            },
            Token {
                type_: LToken::EqSign,
                src_loc: SrcLoc { line: 1, col: 3 },
            },
            Token {
                type_: LToken::WhiteSpace { c: ' ' },
                src_loc: SrcLoc { line: 1, col: 4 },
            },
            Token {
                type_: LToken::IntTok { v: 1 },
                src_loc: SrcLoc { line: 1, col: 5 },
            },
            Token {
                type_: LToken::SemiColon,
                src_loc: SrcLoc { line: 1, col: 6 },
            },
        ];
        let (_, stmt) = parse_stmt(&input).unwrap();
        assert_eq!(
            stmt,
            Stmt::Assign {
                x: Var("a".to_string()),
                e: Exp::ConstExp {
                    l: Const::IntConst { v: 1 }
                }
            }
        );
    }

    #[test]
    fn test_parse_assign_simple() {
        let input = vec![
            Token {
                type_: LToken::IdTok { v: "a".to_string() },
                src_loc: SrcLoc { line: 1, col: 1 },
            },
            Token {
                type_: LToken::EqSign,
                src_loc: SrcLoc { line: 1, col: 2 },
            },
            Token {
                type_: LToken::IntTok { v: 1 },
                src_loc: SrcLoc { line: 1, col: 3 },
            },
            Token {
                type_: LToken::SemiColon,
                src_loc: SrcLoc { line: 1, col: 4 },
            },
        ];
        let (_, stmt) = parse_stmt(&input).unwrap();
        assert_eq!(
            stmt,
            Stmt::Assign {
                x: Var("a".to_string()),
                e: Exp::ConstExp {
                    l: Const::IntConst { v: 1 }
                }
            }
        );
    }
    #[test]
    fn test_parse_exp() {
        let input = vec![Token {
            type_: LToken::IntTok { v: 1 },
            src_loc: SrcLoc { line: 1, col: 1 },
        }];
        let (_, exp) = parse_exp(&input).unwrap();
        assert_eq!(
            exp,
            Exp::ConstExp {
                l: Const::IntConst { v: 1 }
            }
        );
    }

    #[test]
    fn test_parse_exp_left_rec() {
        let input = vec![Token {
            type_: LToken::IntTok { v: 1 },
            src_loc: SrcLoc { line: 1, col: 1 },
        }];
        let (_, exp_le) = parse_exp_left_rec(&input).unwrap();
        assert_eq!(
            exp_le,
            ExpLE {
                t: Term {
                    f: Factor::ConstExpP {
                        l: Const::IntConst { v: 1 }
                    },
                    t_p: TermP::EpsTermP
                },
                e_p: ExpLEP::EpsExpLEP
            }
        );
    }

    #[test]
    fn test_parse_term() {
        let input = vec![Token {
            type_: LToken::IntTok { v: 1 },
            src_loc: SrcLoc { line: 1, col: 1 },
        }];
        let (_, term) = parse_term(&input).unwrap();
        assert_eq!(
            term,
            Term {
                f: Factor::ConstExpP {
                    l: Const::IntConst { v: 1 }
                },
                t_p: TermP::EpsTermP
            }
        );
    }

    #[test]
    fn test_parse_term_p() {
        let input = vec![];
        let (_, term_p) = parse_term_p(&input).unwrap();
        assert_eq!(term_p, TermP::EpsTermP);
    }

    #[test]
    fn test_parse_const() {
        let input = vec![Token {
            type_: LToken::IntTok { v: 1 },
            src_loc: SrcLoc { line: 1, col: 1 },
        }];
        let (_, factor) = parse_factor(&input).unwrap();
        assert_eq!(
            factor,
            Factor::ConstExpP {
                l: Const::IntConst { v: 1 }
            }
        );
    }

    #[test]
    fn test_parse_exp_p() {
        let input = vec![];
        let (_, exp_p) = parse_exp_p(&input).unwrap();
        assert_eq!(exp_p, ExpLEP::EpsExpLEP);
    }

    #[test]
    fn test_parse_multiplication() {
        let input = vec![
            Token {
                type_: LToken::AsteriskSign,
                src_loc: SrcLoc { line: 1, col: 1 },
            },
            Token {
                type_: LToken::IntTok { v: 1 },
                src_loc: SrcLoc { line: 1, col: 2 },
            },
        ];
        let (_, term_p) = parse_multiplication(&input).unwrap();
        assert_eq!(
            term_p,
            TermP::MultP {
                f: Factor::ConstExpP {
                    l: Const::IntConst { v: 1 }
                },
                t_p: Box::new(TermP::EpsTermP)
            }
        );
    }
    #[test]
    fn test_parse_spaces() {
        let input = vec![
            Token {
                type_: LToken::WhiteSpace { c: '\n' },
                src_loc: SrcLoc { line: 1, col: 1 },
            },
            Token {
                type_: LToken::WhiteSpace { c: ' ' },
                src_loc: SrcLoc { line: 1, col: 2 },
            },
            Token {
                type_: LToken::IntTok { v: 1 },
                src_loc: SrcLoc { line: 1, col: 3 },
            },
        ];
        let (remaining_input, _) = parse_spaces(&input).unwrap();
        assert_eq!(remaining_input.len(), 1);
        assert_eq!(remaining_input[0].type_, LToken::IntTok { v: 1 });
    }

    #[test]
    fn test_parser_parsing_y_equals_1_x_equals_x_plus_1() {
        // y = 1;\n x = x + 1;
        let input = vec![
            Token {
                type_: LToken::IdTok { v: "y".to_string() },
                src_loc: SrcLoc { line: 1, col: 2 },
            },
            Token {
                type_: LToken::WhiteSpace { c: ' ' },
                src_loc: SrcLoc { line: 1, col: 3 },
            },
            Token {
                type_: LToken::EqSign,
                src_loc: SrcLoc { line: 1, col: 4 },
            },
            Token {
                type_: LToken::WhiteSpace { c: ' ' },
                src_loc: SrcLoc { line: 1, col: 5 },
            },
            Token {
                type_: LToken::IntTok { v: 1 },
                src_loc: SrcLoc { line: 1, col: 6 },
            },
            Token {
                type_: LToken::SemiColon,
                src_loc: SrcLoc { line: 1, col: 7 },
            },
            Token {
                type_: LToken::WhiteSpace { c: '\n' },
                src_loc: SrcLoc { line: 1, col: 8 },
            },
            Token {
                type_: LToken::IdTok { v: "x".to_string() },
                src_loc: SrcLoc { line: 1, col: 9 },
            },
            Token {
                type_: LToken::WhiteSpace { c: ' ' },
                src_loc: SrcLoc { line: 1, col: 10 },
            },
            Token {
                type_: LToken::EqSign,
                src_loc: SrcLoc { line: 1, col: 11 },
            },
            Token {
                type_: LToken::WhiteSpace { c: ' ' },
                src_loc: SrcLoc { line: 1, col: 12 },
            },
            Token {
                type_: LToken::IdTok { v: "x".to_string() },
                src_loc: SrcLoc { line: 1, col: 13 },
            },
            Token {
                type_: LToken::WhiteSpace { c: ' ' },
                src_loc: SrcLoc { line: 1, col: 14 },
            },
            Token {
                type_: LToken::PlusSign,
                src_loc: SrcLoc { line: 1, col: 15 },
            },
            Token {
                type_: LToken::WhiteSpace { c: ' ' },
                src_loc: SrcLoc { line: 1, col: 16 },
            },
            Token {
                type_: LToken::IntTok { v: 1 },
                src_loc: SrcLoc { line: 1, col: 17 },
            },
            Token {
                type_: LToken::SemiColon,
                src_loc: SrcLoc { line: 1, col: 18 },
            },
        ];

        let expected = vec![
            Stmt::Assign {
                x: Var("y".to_string()),
                e: Exp::ConstExp {
                    l: Const::IntConst { v: 1 },
                },
            },
            Stmt::Assign {
                x: Var("x".to_string()),
                e: Exp::Plus {
                    e1: Box::new(Exp::VarExp {
                        v: Var("x".to_string()),
                    }),
                    e2: Box::new(Exp::ConstExp {
                        l: Const::IntConst { v: 1 },
                    }),
                },
            },
        ];

        let (remaining, stmts) = parse_stmts(&input).unwrap();
        assert_eq!(remaining.len(), 0);
        assert_eq!(stmts, expected);
    }

    #[test]
    fn test_p_exp_parsing_x_minus_1_plus_y_mult_2() {
        // x - (1 + (y * 2))
        let input = vec![
            Token {
                type_: LToken::IdTok { v: "x".to_string() },
                src_loc: SrcLoc { line: 1, col: 1 },
            },
            Token {
                type_: LToken::WhiteSpace { c: ' ' },
                src_loc: SrcLoc { line: 1, col: 2 },
            },
            Token {
                type_: LToken::MinusSign,
                src_loc: SrcLoc { line: 1, col: 3 },
            },
            Token {
                type_: LToken::WhiteSpace { c: ' ' },
                src_loc: SrcLoc { line: 1, col: 4 },
            },
            Token {
                type_: LToken::LParen,
                src_loc: SrcLoc { line: 1, col: 5 },
            },
            Token {
                type_: LToken::IntTok { v: 1 },
                src_loc: SrcLoc { line: 1, col: 6 },
            },
            Token {
                type_: LToken::WhiteSpace { c: ' ' },
                src_loc: SrcLoc { line: 1, col: 7 },
            },
            Token {
                type_: LToken::PlusSign,
                src_loc: SrcLoc { line: 1, col: 8 },
            },
            Token {
                type_: LToken::WhiteSpace { c: ' ' },
                src_loc: SrcLoc { line: 1, col: 9 },
            },
            Token {
                type_: LToken::LParen,
                src_loc: SrcLoc { line: 1, col: 10 },
            },
            Token {
                type_: LToken::IdTok { v: "y".to_string() },
                src_loc: SrcLoc { line: 1, col: 11 },
            },
            Token {
                type_: LToken::WhiteSpace { c: ' ' },
                src_loc: SrcLoc { line: 1, col: 12 },
            },
            Token {
                type_: LToken::AsteriskSign,
                src_loc: SrcLoc { line: 1, col: 13 },
            },
            Token {
                type_: LToken::WhiteSpace { c: ' ' },
                src_loc: SrcLoc { line: 1, col: 14 },
            },
            Token {
                type_: LToken::IntTok { v: 2 },
                src_loc: SrcLoc { line: 1, col: 15 },
            },
            Token {
                type_: LToken::RParen,
                src_loc: SrcLoc { line: 1, col: 16 },
            },
            Token {
                type_: LToken::RParen,
                src_loc: SrcLoc { line: 1, col: 17 },
            },
        ];

        let expected = Exp::Minus {
            e1: Box::new(Exp::VarExp {
                v: Var("x".to_string()),
            }),
            e2: Box::new(Exp::ParenExp {
                e: Box::new(Exp::Plus {
                    e1: Box::new(Exp::ConstExp {
                        l: Const::IntConst { v: 1 },
                    }),
                    e2: Box::new(Exp::ParenExp {
                        e: Box::new(Exp::Mult {
                            e1: Box::new(Exp::VarExp {
                                v: Var("y".to_string()),
                            }),
                            e2: Box::new(Exp::ConstExp {
                                l: Const::IntConst { v: 2 },
                            }),
                        }),
                    }),
                }),
            }),
        };

        let (_, exp) = parse_exp(&input).unwrap();
        assert_eq!(exp, expected);
    }

    #[test]
    fn test_parser_bigboy() {
        // x = input;
        // s = 0;
        // c = 0;
        // while c < x {
        //     s = c + s;
        //     c = c + 1;
        // }
        // return s;

        let input = vec![
            Token {
                type_: LToken::WhiteSpace { c: '\n' },
                src_loc: SrcLoc { line: 1, col: 1 },
            },
            Token {
                type_: LToken::IdTok { v: "x".to_string() },
                src_loc: SrcLoc { line: 2, col: 1 },
            },
            Token {
                type_: LToken::WhiteSpace { c: ' ' },
                src_loc: SrcLoc { line: 2, col: 2 },
            },
            Token {
                type_: LToken::EqSign,
                src_loc: SrcLoc { line: 2, col: 3 },
            },
            Token {
                type_: LToken::WhiteSpace { c: ' ' },
                src_loc: SrcLoc { line: 2, col: 4 },
            },
            Token {
                type_: LToken::IdTok {
                    v: "input".to_string(),
                },
                src_loc: SrcLoc { line: 2, col: 5 },
            },
            Token {
                type_: LToken::SemiColon,
                src_loc: SrcLoc { line: 2, col: 10 },
            },
            Token {
                type_: LToken::WhiteSpace { c: '\n' },
                src_loc: SrcLoc { line: 2, col: 11 },
            },
            Token {
                type_: LToken::IdTok { v: "s".to_string() },
                src_loc: SrcLoc { line: 3, col: 1 },
            },
            Token {
                type_: LToken::WhiteSpace { c: ' ' },
                src_loc: SrcLoc { line: 3, col: 2 },
            },
            Token {
                type_: LToken::EqSign,
                src_loc: SrcLoc { line: 3, col: 3 },
            },
            Token {
                type_: LToken::WhiteSpace { c: ' ' },
                src_loc: SrcLoc { line: 3, col: 4 },
            },
            Token {
                type_: LToken::IntTok { v: 0 },
                src_loc: SrcLoc { line: 3, col: 5 },
            },
            Token {
                type_: LToken::SemiColon,
                src_loc: SrcLoc { line: 3, col: 6 },
            },
            Token {
                type_: LToken::WhiteSpace { c: '\n' },
                src_loc: SrcLoc { line: 3, col: 7 },
            },
            Token {
                type_: LToken::IdTok { v: "c".to_string() },
                src_loc: SrcLoc { line: 4, col: 1 },
            },
            Token {
                type_: LToken::WhiteSpace { c: ' ' },
                src_loc: SrcLoc { line: 4, col: 2 },
            },
            Token {
                type_: LToken::EqSign,
                src_loc: SrcLoc { line: 4, col: 3 },
            },
            Token {
                type_: LToken::WhiteSpace { c: ' ' },
                src_loc: SrcLoc { line: 4, col: 4 },
            },
            Token {
                type_: LToken::IntTok { v: 0 },
                src_loc: SrcLoc { line: 4, col: 5 },
            },
            Token {
                type_: LToken::SemiColon,
                src_loc: SrcLoc { line: 4, col: 6 },
            },
            Token {
                type_: LToken::WhiteSpace { c: '\n' },
                src_loc: SrcLoc { line: 4, col: 7 },
            },
            Token {
                type_: LToken::WhileKW,
                src_loc: SrcLoc { line: 5, col: 1 },
            },
            Token {
                type_: LToken::WhiteSpace { c: ' ' },
                src_loc: SrcLoc { line: 5, col: 6 },
            },
            Token {
                type_: LToken::IdTok { v: "c".to_string() },
                src_loc: SrcLoc { line: 5, col: 7 },
            },
            Token {
                type_: LToken::WhiteSpace { c: ' ' },
                src_loc: SrcLoc { line: 5, col: 8 },
            },
            Token {
                type_: LToken::LThanSign,
                src_loc: SrcLoc { line: 5, col: 9 },
            },
            Token {
                type_: LToken::WhiteSpace { c: ' ' },
                src_loc: SrcLoc { line: 5, col: 10 },
            },
            Token {
                type_: LToken::IdTok { v: "x".to_string() },
                src_loc: SrcLoc { line: 5, col: 11 },
            },
            Token {
                type_: LToken::WhiteSpace { c: ' ' },
                src_loc: SrcLoc { line: 5, col: 12 },
            },
            Token {
                type_: LToken::LBrace,
                src_loc: SrcLoc { line: 5, col: 13 },
            },
            Token {
                type_: LToken::WhiteSpace { c: '\n' },
                src_loc: SrcLoc { line: 5, col: 14 },
            },
            Token {
                type_: LToken::WhiteSpace { c: '\t' },
                src_loc: SrcLoc { line: 6, col: 1 },
            },
            Token {
                type_: LToken::IdTok { v: "s".to_string() },
                src_loc: SrcLoc { line: 6, col: 2 },
            },
            Token {
                type_: LToken::WhiteSpace { c: ' ' },
                src_loc: SrcLoc { line: 6, col: 3 },
            },
            Token {
                type_: LToken::EqSign,
                src_loc: SrcLoc { line: 6, col: 4 },
            },
            Token {
                type_: LToken::WhiteSpace { c: ' ' },
                src_loc: SrcLoc { line: 6, col: 5 },
            },
            Token {
                type_: LToken::IdTok { v: "s".to_string() },
                src_loc: SrcLoc { line: 6, col: 6 },
            },
            Token {
                type_: LToken::WhiteSpace { c: ' ' },
                src_loc: SrcLoc { line: 6, col: 7 },
            },
            Token {
                type_: LToken::PlusSign,
                src_loc: SrcLoc { line: 6, col: 8 },
            },
            Token {
                type_: LToken::WhiteSpace { c: ' ' },
                src_loc: SrcLoc { line: 6, col: 9 },
            },
            Token {
                type_: LToken::IdTok { v: "c".to_string() },
                src_loc: SrcLoc { line: 6, col: 10 },
            },
            Token {
                type_: LToken::SemiColon,
                src_loc: SrcLoc { line: 6, col: 11 },
            },
            Token {
                type_: LToken::WhiteSpace { c: '\n' },
                src_loc: SrcLoc { line: 6, col: 12 },
            },
            Token {
                type_: LToken::WhiteSpace { c: '\t' },
                src_loc: SrcLoc { line: 7, col: 1 },
            },
            Token {
                type_: LToken::IdTok { v: "c".to_string() },
                src_loc: SrcLoc { line: 7, col: 2 },
            },
            Token {
                type_: LToken::WhiteSpace { c: ' ' },
                src_loc: SrcLoc { line: 7, col: 3 },
            },
            Token {
                type_: LToken::EqSign,
                src_loc: SrcLoc { line: 7, col: 4 },
            },
            Token {
                type_: LToken::WhiteSpace { c: ' ' },
                src_loc: SrcLoc { line: 7, col: 5 },
            },
            Token {
                type_: LToken::IdTok { v: "c".to_string() },
                src_loc: SrcLoc { line: 7, col: 6 },
            },
            Token {
                type_: LToken::WhiteSpace { c: ' ' },
                src_loc: SrcLoc { line: 7, col: 7 },
            },
            Token {
                type_: LToken::PlusSign,
                src_loc: SrcLoc { line: 7, col: 8 },
            },
            Token {
                type_: LToken::WhiteSpace { c: ' ' },
                src_loc: SrcLoc { line: 7, col: 9 },
            },
            Token {
                type_: LToken::IntTok { v: 1 },
                src_loc: SrcLoc { line: 7, col: 10 },
            },
            Token {
                type_: LToken::SemiColon,
                src_loc: SrcLoc { line: 7, col: 11 },
            },
            Token {
                type_: LToken::WhiteSpace { c: '\n' },
                src_loc: SrcLoc { line: 7, col: 12 },
            },
            Token {
                type_: LToken::RBrace,
                src_loc: SrcLoc { line: 8, col: 1 },
            },
            Token {
                type_: LToken::WhiteSpace { c: '\n' },
                src_loc: SrcLoc { line: 8, col: 2 },
            },
            Token {
                type_: LToken::RetKW,
                src_loc: SrcLoc { line: 9, col: 1 },
            },
            Token {
                type_: LToken::WhiteSpace { c: ' ' },
                src_loc: SrcLoc { line: 9, col: 7 },
            },
            Token {
                type_: LToken::IdTok { v: "s".to_string() },
                src_loc: SrcLoc { line: 9, col: 8 },
            },
            Token {
                type_: LToken::SemiColon,
                src_loc: SrcLoc { line: 9, col: 9 },
            },
        ];
        let expected = vec![
            Stmt::Assign {
                x: Var("x".to_string()),
                e: Exp::VarExp {
                    v: Var("input".to_string()),
                },
            },
            Stmt::Assign {
                x: Var("s".to_string()),
                e: Exp::ConstExp {
                    l: Const::IntConst { v: 0 },
                },
            },
            Stmt::Assign {
                x: Var("c".to_string()),
                e: Exp::ConstExp {
                    l: Const::IntConst { v: 0 },
                },
            },
            Stmt::While {
                cond: Exp::LThan {
                    e1: Box::new(Exp::VarExp {
                        v: Var("c".to_string()),
                    }),
                    e2: Box::new(Exp::VarExp {
                        v: Var("x".to_string()),
                    }),
                },
                body: vec![
                    Stmt::Assign {
                        x: Var("s".to_string()),
                        e: Exp::Plus {
                            e1: Box::new(Exp::VarExp {
                                v: Var("s".to_string()),
                            }),
                            e2: Box::new(Exp::VarExp {
                                v: Var("c".to_string()),
                            }),
                        },
                    },
                    Stmt::Assign {
                        x: Var("c".to_string()),
                        e: Exp::Plus {
                            e1: Box::new(Exp::VarExp {
                                v: Var("c".to_string()),
                            }),
                            e2: Box::new(Exp::ConstExp {
                                l: Const::IntConst { v: 1 },
                            }),
                        },
                    },
                ],
            },
            Stmt::Ret {
                x: Var("s".to_string()),
            },
        ];
        let (remaining, stmts) = parse_stmts(&input).unwrap();
        assert_eq!(stmts, expected);
        assert_eq!(remaining.len(), 0);
    }
}
