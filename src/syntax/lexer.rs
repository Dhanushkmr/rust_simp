use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, digit1, one_of},
    combinator::{map_res, recognize},
    multi::many0,
    sequence::pair,
    IResult,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SrcLoc {
    pub line: usize,
    pub col: usize,
}

impl SrcLoc {
    fn new() -> Self {
        SrcLoc { line: 1, col: 1 }
    }

    fn update(&mut self, consumed: &str) {
        for c in consumed.chars() {
            if c == '\n' {
                self.line += 1;
                self.col = 1;
            } else {
                self.col += 1;
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum LToken {
    // Operators
    EqSign,
    DEqSign,
    PlusSign,
    MinusSign,
    AsteriskSign,
    FSlashSign,
    LThanSign,
    // Punctuation
    LBrace,
    RBrace,
    LParen,
    RParen,
    SemiColon,
    // Keywords
    RetKW,
    IfKW,
    ElseKW,
    WhileKW,
    NopKW,
    TrueKW,
    FalseKW,
    // Literals and whitespace
    IdTok { v: String },
    IntTok { v: usize },
    WhiteSpace { c: char },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub type_: LToken,
    pub src_loc: SrcLoc,
}

fn calculate_src_loc(input: &str, remaining: &str, src_loc: &mut SrcLoc) {
    let consumed = &input[..input.len() - remaining.len()];
    src_loc.update(consumed);
}

fn parse_eq_sign(input: &str) -> IResult<&str, LToken> {
    let (remaining, _) = tag("=")(input)?;
    Ok((remaining, LToken::EqSign))
}

fn parse_deq_sign(input: &str) -> IResult<&str, LToken> {
    let (remaining, _) = tag("==")(input)?;
    Ok((remaining, LToken::DEqSign))
}

fn parse_plus_sign(input: &str) -> IResult<&str, LToken> {
    let (remaining, _) = tag("+")(input)?;
    Ok((remaining, LToken::PlusSign))
}

fn parse_minus_sign(input: &str) -> IResult<&str, LToken> {
    let (remaining, _) = tag("-")(input)?;
    Ok((remaining, LToken::MinusSign))
}

fn parse_asterisk_sign(input: &str) -> IResult<&str, LToken> {
    let (remaining, _) = tag("*")(input)?;
    Ok((remaining, LToken::AsteriskSign))
}

fn parse_fslash_sign(input: &str) -> IResult<&str, LToken> {
    let (remaining, _) = tag("/")(input)?;
    Ok((remaining, LToken::FSlashSign))
}

fn parse_lthan_sign(input: &str) -> IResult<&str, LToken> {
    let (remaining, _) = tag("<")(input)?;
    Ok((remaining, LToken::LThanSign))
}

fn parse_lbrace_sign(input: &str) -> IResult<&str, LToken> {
    let (remaining, _) = tag("{")(input)?;
    Ok((remaining, LToken::LBrace))
}

fn parse_rbrace_sign(input: &str) -> IResult<&str, LToken> {
    let (remaining, _) = tag("}")(input)?;
    Ok((remaining, LToken::RBrace))
}

fn parse_lparen_sign(input: &str) -> IResult<&str, LToken> {
    let (remaining, _) = tag("(")(input)?;
    Ok((remaining, LToken::LParen))
}

fn parse_rparen_sign(input: &str) -> IResult<&str, LToken> {
    let (remaining, _) = tag(")")(input)?;
    Ok((remaining, LToken::RParen))
}

fn parse_semicolon_sign(input: &str) -> IResult<&str, LToken> {
    let (remaining, _) = tag(";")(input)?;
    Ok((remaining, LToken::SemiColon))
}

fn parse_return(input: &str) -> IResult<&str, LToken> {
    let (remaining, _) = tag("return")(input)?;
    Ok((remaining, LToken::RetKW))
}

fn parse_if(input: &str) -> IResult<&str, LToken> {
    let (remaining, _) = tag("if")(input)?;
    Ok((remaining, LToken::IfKW))
}

fn parse_else(input: &str) -> IResult<&str, LToken> {
    let (remaining, _) = tag("else")(input)?;
    Ok((remaining, LToken::ElseKW))
}

fn parse_while(input: &str) -> IResult<&str, LToken> {
    let (remaining, _) = tag("while")(input)?;
    Ok((remaining, LToken::WhileKW))
}

fn parse_nop(input: &str) -> IResult<&str, LToken> {
    let (remaining, _) = tag("nop")(input)?;
    Ok((remaining, LToken::NopKW))
}

fn parse_true(input: &str) -> IResult<&str, LToken> {
    let (remaining, _) = tag("true")(input)?;
    Ok((remaining, LToken::TrueKW))
}

fn parse_false(input: &str) -> IResult<&str, LToken> {
    let (remaining, _) = tag("false")(input)?;
    Ok((remaining, LToken::FalseKW))
}

fn parse_int(input: &str) -> IResult<&str, LToken> {
    let (remaining, value) = map_res(digit1, |s: &str| s.parse::<usize>())(input)?;
    let token = LToken::IntTok { v: value };
    Ok((remaining, token))
}

fn parse_id(input: &str) -> IResult<&str, LToken> {
    let (remaining, value) = recognize(pair(alpha1, many0(alt((alphanumeric1, tag("_"))))))(input)?;
    let token = LToken::IdTok {
        v: value.to_string(),
    };
    Ok((remaining, token))
}

fn parse_whitespace(input: &str) -> IResult<&str, LToken> {
    let (remaining, value) = one_of(" \t\r\n")(input)?;
    let token = LToken::WhiteSpace { c: value };
    Ok((remaining, token))
}

fn parse_token<'a>(input: &'a str, src_loc: &mut SrcLoc) -> IResult<&'a str, Token> {
    let mut temp_loc = src_loc.clone();
    calculate_src_loc(input, input, &mut temp_loc);

    let (remaining, token) = alt((
        parse_deq_sign,
        parse_eq_sign,
        parse_plus_sign,
        parse_minus_sign,
        parse_asterisk_sign,
        parse_fslash_sign,
        parse_lthan_sign,
        parse_lbrace_sign,
        parse_rbrace_sign,
        parse_lparen_sign,
        parse_rparen_sign,
        parse_semicolon_sign,
        parse_return,
        parse_if,
        parse_else,
        parse_while,
        parse_nop,
        parse_true,
        parse_false,
        parse_int,
        alt((parse_id, parse_whitespace)),
    ))(input)?;

    calculate_src_loc(input, remaining, src_loc);

    let token = Token {
        type_: token,
        src_loc: temp_loc,
    };
    Ok((remaining, token))
}

fn parse_tokens(input: &str) -> IResult<&str, Vec<Token>> {
    let mut src_loc = SrcLoc::new();
    let mut remaining = input;
    let mut tokens = Vec::new();

    while !remaining.is_empty() {
        let (new_remaining, token) = parse_token(remaining, &mut src_loc)?;
        tokens.push(token);
        remaining = new_remaining;
    }

    Ok((remaining, tokens))
}

#[cfg(test)]
use pretty_assertions::assert_eq;
mod tests {
    use super::*;

    #[test]
    fn test_basic_nom_string() {
        let input = "true";
        let expected = Ok(("", LToken::TrueKW));
        let actual = parse_true(input);
        self::assert_eq!(actual, expected);
    }

    #[test]
    fn test_simple_assignment() {
        let input = "a = 1;";
        let expected = vec![
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
        let (remaining, tokens) = parse_tokens(input).unwrap();
        self::assert_eq!(remaining, "");
        self::assert_eq!(tokens, expected);
    }

    #[test]
    fn test_assign_input() {
        let input = "y = input; ";
        let expected = vec![
            Token {
                type_: LToken::IdTok { v: "y".to_string() },
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
                type_: LToken::IdTok {
                    v: "input".to_string(),
                },
                src_loc: SrcLoc { line: 1, col: 5 },
            },
            Token {
                type_: LToken::SemiColon,
                src_loc: SrcLoc { line: 1, col: 10 },
            },
            Token {
                type_: LToken::WhiteSpace { c: ' ' },
                src_loc: SrcLoc { line: 1, col: 11 },
            },
        ];
        let (remaining, tokens) = parse_tokens(input).unwrap();
        self::assert_eq!(remaining, "");
        self::assert_eq!(tokens, expected);
    }

    #[test]
    fn test_multi_assignment() {
        let input = "a = 1; b = 2; c = 3;";
        let expected = vec![
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
            Token {
                type_: LToken::WhiteSpace { c: ' ' },
                src_loc: SrcLoc { line: 1, col: 7 },
            },
            Token {
                type_: LToken::IdTok { v: "b".to_string() },
                src_loc: SrcLoc { line: 1, col: 8 },
            },
            Token {
                type_: LToken::WhiteSpace { c: ' ' },
                src_loc: SrcLoc { line: 1, col: 9 },
            },
            Token {
                type_: LToken::EqSign,
                src_loc: SrcLoc { line: 1, col: 10 },
            },
            Token {
                type_: LToken::WhiteSpace { c: ' ' },
                src_loc: SrcLoc { line: 1, col: 11 },
            },
            Token {
                type_: LToken::IntTok { v: 2 },
                src_loc: SrcLoc { line: 1, col: 12 },
            },
            Token {
                type_: LToken::SemiColon,
                src_loc: SrcLoc { line: 1, col: 13 },
            },
            Token {
                type_: LToken::WhiteSpace { c: ' ' },
                src_loc: SrcLoc { line: 1, col: 14 },
            },
            Token {
                type_: LToken::IdTok { v: "c".to_string() },
                src_loc: SrcLoc { line: 1, col: 15 },
            },
            Token {
                type_: LToken::WhiteSpace { c: ' ' },
                src_loc: SrcLoc { line: 1, col: 16 },
            },
            Token {
                type_: LToken::EqSign,
                src_loc: SrcLoc { line: 1, col: 17 },
            },
            Token {
                type_: LToken::WhiteSpace { c: ' ' },
                src_loc: SrcLoc { line: 1, col: 18 },
            },
            Token {
                type_: LToken::IntTok { v: 3 },
                src_loc: SrcLoc { line: 1, col: 19 },
            },
            Token {
                type_: LToken::SemiColon,
                src_loc: SrcLoc { line: 1, col: 20 },
            },
        ];
        let (remaining, tokens) = parse_tokens(input).unwrap();
        self::assert_eq!(remaining, "");
        self::assert_eq!(tokens, expected);
    }

    #[test]
    fn test_full_lexer() {
        let input =
            "\nx = input;\ns = 0;\nc = 0;\nwhile c < x {\n\ts = s + c;\n\tc = c + 1;\n}\nreturn s;";
        let expected = vec![
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
        let (remaining, tokens) = parse_tokens(input).unwrap();
        self::assert_eq!(remaining, "");
        self::assert_eq!(tokens, expected);
    }
}
