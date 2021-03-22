use super::*;
use lex::{Terminal, Token as LToken, TokenVal as LTokenVal, Tokens as LTokens};

use log::info;
use thiserror::Error;

const HEUR: usize = 2;

#[derive(Debug, Error)]
pub enum ParseErrorType {
    #[error("Expected comma after bare label")]
    ExpectedComma,

    #[error("Cannot place multiple labels on the same memory location: {0} and {1}")]
    MultiLabel(Span, Span),

    #[error("Missing reference for instruction {0}")]
    MissingReference(&'static str),

    #[error("Memory operation {0} expected indirection argument found {1}")]
    ExpectedIndirection(&'static str, Span),

    #[error("Instruction {0} did not expect any arguments, found {1}")]
    NoArgumentsExpected(&'static str, Span),

    #[error("Expected literal value after directive")]
    DirectiveLiteralMissing,

    #[error("Could not parse value {0} as Hex literal")]
    LiteralHexValueFormat(Span),

    #[error("Could not parse value {0} as Decimal literal")]
    LiteralDecValueFormat(Span),

    #[error("Did not expect the start of instruction {0}")]
    UnexpectedToken(Span),
}

#[derive(Debug)]
pub struct ParseError<'a> {
    ctx: ParseContext<'a>,
    span: Span,
    ty: ParseErrorType,
}

impl<'a> std::fmt::Display for ParseError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(line) = self.span.line {
            writeln!(f, "Encountered Error on line {}", line)?;
        } else {
            writeln!(f, "Encountered Error")?;
        }

        writeln!(f, "{}", self.ty)?;

        if let Some(ls) = self.span.line.and_then(|a| self.ctx.lines.get(&(a as u32))) {
            writeln!(f, "{}", self.span.red_in(self.ctx.instr, *ls))
        } else {
            writeln!(f, "{}", self.span.red(self.ctx.instr))
        }
    }
}

impl<'a> std::error::Error for ParseError<'a> {}

#[derive(Debug)]
pub enum ReferenceToken {
    LabelDef(Span, Box<ReferenceToken>),

    And(Span, bool),
    Add(Span, bool),
    Lda(Span, bool),
    Sta(Span, bool),
    Bun(Span, bool),
    Bsa(Span, bool),
    Isz(Span, bool),

    Cla,
    Cle,
    Cma,
    Cme,
    Cir,
    Cil,
    Inc,
    Spa,
    Sna,
    Sze,
    Hlt,

    // IO ops
    Inp,
    Out,
    Ski,
    Sko,
    Ion,
    Iof,

    Org(u16),
    Hex(u16),
    Dec(i16),
}

#[derive(Debug)]
pub struct TokenTree<'a> {
    ctx: ParseContext<'a>,
    tokens: Box<[ReferenceToken]>,
}

fn memory_op<'l, 'c, 'a>(
    left: &'l [LToken],
    ctx: &'c ParseContext<'a>,
    ins: &'static str,
) -> Result<'a, (&'l [LToken], Span, bool)> {
    if left[1].tval == LTokenVal::NonTerminal {
        match left[2].tval {
            LTokenVal::Terminal(Terminal::Indirection) => Ok((&left[3..], left[1].span, true)),
            LTokenVal::Terminal(Terminal::CommentStart)
            | LTokenVal::Terminal(Terminal::Newline) => Ok((&left[2..], left[1].span, false)),
            _ => Err(ParseError {
                ctx: ctx.clone(),
                span: left[0].span.join(left[2].span),
                ty: ParseErrorType::ExpectedIndirection(ins, left[2].span),
            })?,
        }
    } else {
        Err(ParseError {
            ctx: ctx.clone(),
            span: left[0].span.join(left[2].span),
            ty: ParseErrorType::MissingReference(ins),
        })?
    }
}

fn reg_io_op<'l, 'c, 'a>(
    left: &'l [LToken],
    ctx: &'c ParseContext<'a>,
    ins: &'static str,
) -> Result<'a, &'l [LToken]> {
    match left[1].tval {
        LTokenVal::Terminal(Terminal::CommentStart) | LTokenVal::Terminal(Terminal::Newline) => {
            Ok(&left[1..])
        }
        _ => Err(ParseError {
            ctx: ctx.clone(),
            span: left[0].span.join(left[1].span),
            ty: ParseErrorType::NoArgumentsExpected(ins, left[1].span),
        })?,
    }
}

/// Eat comment and newline chars
fn eat_nl_com<'l, 'c, 'a>(
    mut left: &'l [LToken],
    ctx: &'c ParseContext<'a>,
    last_s: Span,
) -> Result<'a, &'l [LToken]> {
    let mut ate = 0;

    loop {
        // If we are eating the last line
        if left.len() == 0 {
            return Ok(left);
        }

        match left[0].tval {
            LTokenVal::Terminal(Terminal::Newline) => {
                ate += 1;
                left = &left[1..];
            }
            LTokenVal::Terminal(Terminal::CommentStart) | LTokenVal::Ignored => {
                left = &left[1..];
            }
            _ => break,
        }
    }

    if ate != 0 {
        Ok(left)
    } else {
        Err(ParseError {
            ctx: ctx.clone(),
            span: last_s.join(left[0].span),
            ty: ParseErrorType::UnexpectedToken(left[0].span),
        })
        .map_err(AssembleError::from)
    }
}

fn label(o: ReferenceToken, lab: &mut Option<Span>) -> ReferenceToken {
    if let Some(lab) = lab.take() {
        ReferenceToken::LabelDef(lab, Box::new(o))
    } else {
        o
    }
}

pub fn parse(
    LTokens {
        ctx,
        tokens: ltokens,
    }: LTokens,
) -> Result<TokenTree> {
    info!("Beginning parse step with heur of {}", HEUR);

    dbg!(&ctx, &ltokens);
    let len = ltokens.len();

    let mut tokens = Vec::with_capacity(len / HEUR);

    // Eat blank lines or comment lines at beginning of file
    let mut left = eat_nl_com(&*ltokens, &ctx, ltokens[0].span).unwrap_or_else(|_| &*ltokens);

    let mut lab: Option<Span> = None;

    while left.len() > 0 {
        match left[0].tval {
            LTokenVal::NonTerminal => {
                if left[1].tval == LTokenVal::Terminal(Terminal::Comma) {
                    if let Some(sp) = lab {
                        Err(ParseError {
                            ctx: ctx.clone(),
                            span: sp.join(left[0].span),
                            ty: ParseErrorType::MultiLabel(sp, left[0].span),
                        })?;
                    } else {
                        lab = Some(left[0].span);
                        left = &left[2..];
                    }
                } else {
                    Err(ParseError {
                        ctx: ctx.clone(),
                        span: left[0].span.join(left[1].span),
                        ty: ParseErrorType::ExpectedComma,
                    })?;
                }
            }

            LTokenVal::Ignored => {
                unreachable!(
                    "Comment values should only ever be handled in the CommentStart terminal case"
                );
            }

            LTokenVal::Terminal(t) => match t {
                Terminal::And => {
                    let (nl, s, b) = memory_op(left, &ctx, "AND")?;

                    left = eat_nl_com(nl, &ctx, s)?;
                    tokens.push(label(ReferenceToken::And(s, b), &mut lab));
                }

                Terminal::Add => {
                    let (nl, s, b) = memory_op(left, &ctx, "ADD")?;

                    left = eat_nl_com(nl, &ctx, s)?;
                    tokens.push(label(ReferenceToken::Add(s, b), &mut lab));
                }

                Terminal::Lda => {
                    let (nl, s, b) = memory_op(left, &ctx, "LDA")?;

                    left = eat_nl_com(nl, &ctx, s)?;
                    tokens.push(label(ReferenceToken::Lda(s, b), &mut lab));
                }

                Terminal::Sta => {
                    let (nl, s, b) = memory_op(left, &ctx, "STA")?;

                    left = eat_nl_com(nl, &ctx, s)?;
                    tokens.push(label(ReferenceToken::Sta(s, b), &mut lab));
                }

                Terminal::Bun => {
                    let (nl, s, b) = memory_op(left, &ctx, "BUN")?;

                    left = eat_nl_com(nl, &ctx, s)?;
                    tokens.push(label(ReferenceToken::Bun(s, b), &mut lab));
                }

                Terminal::Bsa => {
                    let (nl, s, b) = memory_op(left, &ctx, "BSA")?;

                    left = eat_nl_com(nl, &ctx, s)?;
                    tokens.push(label(ReferenceToken::Bsa(s, b), &mut lab));
                }

                Terminal::Isz => {
                    let (nl, s, b) = memory_op(left, &ctx, "ISZ")?;

                    left = eat_nl_com(nl, &ctx, s)?;
                    tokens.push(label(ReferenceToken::Isz(s, b), &mut lab));
                }

                // Register Ops
                Terminal::Cla => {
                    left = eat_nl_com(reg_io_op(left, &ctx, "CLA")?, &ctx, left[0].span)?;
                    tokens.push(label(ReferenceToken::Cla, &mut lab))
                }

                Terminal::Cle => {
                    left = eat_nl_com(reg_io_op(left, &ctx, "CLE")?, &ctx, left[0].span)?;
                    tokens.push(label(ReferenceToken::Cle, &mut lab))
                }

                Terminal::Cma => {
                    left = eat_nl_com(reg_io_op(left, &ctx, "CMA")?, &ctx, left[0].span)?;
                    tokens.push(label(ReferenceToken::Cma, &mut lab))
                }

                Terminal::Cme => {
                    left = eat_nl_com(reg_io_op(left, &ctx, "CME")?, &ctx, left[0].span)?;
                    tokens.push(label(ReferenceToken::Cme, &mut lab))
                }

                Terminal::Cir => {
                    left = eat_nl_com(reg_io_op(left, &ctx, "CIR")?, &ctx, left[0].span)?;
                    tokens.push(label(ReferenceToken::Cir, &mut lab))
                }

                Terminal::Cil => {
                    left = eat_nl_com(reg_io_op(left, &ctx, "Cil")?, &ctx, left[0].span)?;
                    tokens.push(label(ReferenceToken::Cil, &mut lab))
                }

                Terminal::Inc => {
                    left = eat_nl_com(reg_io_op(left, &ctx, "INC")?, &ctx, left[0].span)?;
                    tokens.push(label(ReferenceToken::Inc, &mut lab))
                }

                Terminal::Spa => {
                    left = eat_nl_com(reg_io_op(left, &ctx, "SPA")?, &ctx, left[0].span)?;
                    tokens.push(label(ReferenceToken::Spa, &mut lab))
                }

                Terminal::Sna => {
                    left = eat_nl_com(reg_io_op(left, &ctx, "SNA")?, &ctx, left[0].span)?;
                    tokens.push(label(ReferenceToken::Sna, &mut lab))
                }

                Terminal::Sze => {
                    left = eat_nl_com(reg_io_op(left, &ctx, "SZE")?, &ctx, left[0].span)?;
                    tokens.push(label(ReferenceToken::Sze, &mut lab))
                }

                Terminal::Hlt => {
                    left = eat_nl_com(reg_io_op(left, &ctx, "HLT")?, &ctx, left[0].span)?;
                    tokens.push(label(ReferenceToken::Hlt, &mut lab))
                }

                // IO Ops
                Terminal::Inp => {
                    left = eat_nl_com(reg_io_op(left, &ctx, "INP")?, &ctx, left[0].span)?;
                    tokens.push(label(ReferenceToken::Inp, &mut lab))
                }

                Terminal::Out => {
                    left = eat_nl_com(reg_io_op(left, &ctx, "OUT")?, &ctx, left[0].span)?;
                    tokens.push(label(ReferenceToken::Out, &mut lab))
                }

                Terminal::Ski => {
                    left = eat_nl_com(reg_io_op(left, &ctx, "SKI")?, &ctx, left[0].span)?;
                    tokens.push(label(ReferenceToken::Ski, &mut lab))
                }

                Terminal::Sko => {
                    left = eat_nl_com(reg_io_op(left, &ctx, "SKO")?, &ctx, left[0].span)?;
                    tokens.push(label(ReferenceToken::Sko, &mut lab))
                }

                Terminal::Ion => {
                    left = eat_nl_com(reg_io_op(left, &ctx, "ION")?, &ctx, left[0].span)?;
                    tokens.push(label(ReferenceToken::Ion, &mut lab))
                }

                Terminal::Iof => {
                    left = eat_nl_com(reg_io_op(left, &ctx, "Iof")?, &ctx, left[0].span)?;
                    tokens.push(label(ReferenceToken::Iof, &mut lab))
                }

                // Directives
                Terminal::Org => {
                    if left[1].tval == LTokenVal::NonTerminal {
                        if let Ok(v) = u16::from_str_radix(left[1].span.slice(&ctx.instr), 16) {
                            tokens.push(label(ReferenceToken::Org(v), &mut lab));
                            left = eat_nl_com(&left[2..], &ctx, left[0].span.join(left[1].span))?;
                        } else {
                            return Err(ParseError {
                                ctx,
                                span: left[0].span.join(left[1].span),
                                ty: ParseErrorType::LiteralHexValueFormat(left[1].span),
                            })
                            .map_err(AssembleError::from);
                        }
                    } else {
                        return Err(ParseError {
                            ctx,
                            span: left[0].span,
                            ty: ParseErrorType::DirectiveLiteralMissing,
                        })
                        .map_err(AssembleError::from);
                    }
                }

                Terminal::Hex => {
                    if left[1].tval == LTokenVal::NonTerminal {
                        if let Ok(v) = u16::from_str_radix(left[1].span.slice(ctx.instr), 16) {
                            tokens.push(label(ReferenceToken::Hex(v), &mut lab));
                            left = eat_nl_com(&left[2..], &ctx, left[0].span.join(left[1].span))?;
                        } else {
                            return Err(ParseError {
                                ctx,
                                span: left[0].span.join(left[1].span),
                                ty: ParseErrorType::LiteralHexValueFormat(left[1].span),
                            })
                            .map_err(AssembleError::from);
                        }
                    } else {
                        return Err(ParseError {
                            ctx,
                            span: left[0].span,
                            ty: ParseErrorType::DirectiveLiteralMissing,
                        })
                        .map_err(AssembleError::from);
                    }
                }

                Terminal::Dec => {
                    if left[1].tval == LTokenVal::NonTerminal {
                        // TODO handle negative values directly
                        if let Ok(v) = i16::from_str_radix(left[1].span.slice(ctx.instr), 10) {
                            tokens.push(label(ReferenceToken::Dec(v), &mut lab));
                            left = eat_nl_com(&left[2..], &ctx, left[0].span.join(left[1].span))?;
                        } else {
                            return Err(ParseError {
                                ctx,
                                span: left[0].span.join(left[1].span),
                                ty: ParseErrorType::LiteralHexValueFormat(left[1].span),
                            })
                            .map_err(AssembleError::from);
                        }
                    } else {
                        return Err(ParseError {
                            ctx,
                            span: left[0].span,
                            ty: ParseErrorType::DirectiveLiteralMissing,
                        })
                        .map_err(AssembleError::from);
                    }
                }

                // Catch
                a => unreachable!(
                    "{:?} should have been eaten by previous iteration {}\n{}",
                    a,
                    left[0].span,
                    left[0].span.slice(ctx.instr)
                ),
            },
        }
    }

    info!(
        "Best heur for this run would have been: {:.2}",
        len as f32 / tokens.len() as f32
    );

    info!(
        "Original Cap: {}, Cap: {}, Grew by: {}, wasted cap: {}",
        len / HEUR,
        tokens.capacity(),
        tokens.capacity() - len / HEUR,
        tokens.capacity() - tokens.len()
    );

    Ok(TokenTree {
        ctx,
        tokens: tokens.into_boxed_slice(),
    })
}
