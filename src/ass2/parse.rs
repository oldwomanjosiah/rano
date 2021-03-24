use super::*;
use lex::{Terminal, Token as LToken, TokenVal as LTokenVal, Tokens as LTokens};

use log::info;
use thiserror::Error;

const HEUR: usize = 4;

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

    #[error("Found a bare indirection. Indirections should only be attached to memory operations")]
    BareIndirection,

    #[error(
        "Found bare comma. Commas may only be used to indicate that the previous text was a label"
    )]
    BareComma,

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

        writeln!(f, "{}", self.span.red_in_lines(self.ctx.instr, &self.ctx))
    }
}

impl<'a> std::error::Error for ParseError<'a> {}

#[derive(Debug)]
pub enum ReferenceToken {
    LabelDef(Span, Box<ReferenceInstruction>),

    And(Span, bool),
    Add(Span, bool),
    Lda(Span, bool),
    Sta(Span, bool),
    Bun(Span, bool),
    Bsa(Span, bool),
    Isz(Span, bool),

    Cla(),
    Cle(),
    Cma(),
    Cme(),
    Cir(),
    Cil(),
    Inc(),
    Spa(),
    Sna(),
    Sze(),
    Hlt(),

    // IO ops
    Inp(),
    Out(),
    Ski(),
    Sko(),
    Ion(),
    Iof(),

    Org(u16),
    Hex(u16),
    Dec(i16),
}

#[derive(Debug)]
pub struct ReferenceInstruction {
    pub span: Span,
    pub instr: ReferenceToken,
}

impl ReferenceInstruction {
    fn new_mem<C: Fn(Span, bool) -> ReferenceToken>(
        ins: Span,
        reference: Span,
        ind: bool,
        c: C,
    ) -> ReferenceInstruction {
        ReferenceInstruction {
            span: ins,
            instr: c(reference, ind),
        }
    }

    fn new_reg_iob<C: Fn() -> ReferenceToken>(ins: Span, c: C) -> ReferenceInstruction {
        ReferenceInstruction {
            span: ins,
            instr: c(),
        }
    }

    fn label(self, lab: Option<Span>) -> Self {
        match lab {
            Some(lab) => ReferenceInstruction {
                span: lab.join(self.span),
                instr: ReferenceToken::LabelDef(lab, Box::new(self)),
            },
            None => self,
        }
    }
}

#[derive(Debug)]
pub struct TokenTree<'a> {
    ctx: ParseContext<'a>,
    tokens: Box<[ReferenceInstruction]>,
}

fn mem_op<'l, 'c, 'a, C: Fn(Span, bool) -> ReferenceToken>(
    left: &'l [LToken],
    ctx: &'c ParseContext<'a>,
    ty: C,
    ty_name: &'static str,
) -> Result<'a, (&'l [LToken], ReferenceInstruction)> {
    if left[1].tval != LTokenVal::NonTerminal {
        return Err(ParseError {
            ctx: ctx.clone(),
            span: left[0].span.join(left[2].span),
            ty: ParseErrorType::MissingReference(ty_name),
        })
        .map_err(ParseError::into);
    }

    let indirect = match left[2].tval {
        LTokenVal::Terminal(Terminal::Indirection) => true,
        LTokenVal::Terminal(Terminal::CommentStart) => false,
        LTokenVal::Terminal(Terminal::Newline) => false,
        _ => {
            return Err(ParseError {
                ctx: ctx.clone(),
                span: left[0].span.join(left[2].span),
                ty: ParseErrorType::ExpectedIndirection(ty_name, left[2].span),
            })
            .map_err(ParseError::into);
        }
    };

    if indirect {
        let instr =
            ReferenceInstruction::new_mem(left[0].span.join(left[2].span), left[1].span, true, ty);

        Ok((eat_nl_com(&left[3..], ctx, instr.span)?, instr))
    } else {
        let instr =
            ReferenceInstruction::new_mem(left[0].span.join(left[1].span), left[1].span, false, ty);

        Ok((eat_nl_com(&left[2..], ctx, instr.span)?, instr))
    }
}

fn reg_op<'l, 'c, 'a, C: Fn() -> ReferenceToken>(
    left: &'l [LToken],
    ctx: &'c ParseContext<'a>,
    ty: C,
    ty_name: &'static str,
) -> Result<'a, (&'l [LToken], ReferenceInstruction)> {
    match left[1].tval {
        LTokenVal::Terminal(Terminal::CommentStart) | LTokenVal::Terminal(Terminal::Newline) => {
            let instr = ReferenceInstruction {
                span: left[0].span,
                instr: ty(),
            };
            Ok((eat_nl_com(&left[1..], ctx, instr.span)?, instr))
        }
        _ => Err(ParseError {
            ctx: ctx.clone(),
            span: left[0].span.join(left[1].span),
            ty: ParseErrorType::NoArgumentsExpected(ty_name, left[1].span),
        })
        .map_err(ParseError::into),
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

fn label(o: ReferenceInstruction, lab: &mut Option<Span>) -> ReferenceInstruction {
    if let Some(lab) = lab.take() {
        ReferenceInstruction {
            span: lab.join(o.span),
            instr: ReferenceToken::LabelDef(lab, Box::new(o)),
        }
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
                    let (nl, instr) = mem_op(left, &ctx, ReferenceToken::And, "AND")?;
                    left = nl;
                    tokens.push(instr.label(lab.take()));
                }

                Terminal::Add => {
                    let (nl, instr) = mem_op(left, &ctx, ReferenceToken::Add, "ADD")?;
                    left = nl;
                    tokens.push(instr.label(lab.take()));
                }

                Terminal::Lda => {
                    let (nl, instr) = mem_op(left, &ctx, ReferenceToken::Lda, "LDA")?;
                    left = nl;
                    tokens.push(instr.label(lab.take()));
                }

                Terminal::Sta => {
                    let (nl, instr) = mem_op(left, &ctx, ReferenceToken::Sta, "STA")?;
                    left = nl;
                    tokens.push(instr.label(lab.take()));
                }

                Terminal::Bun => {
                    let (nl, instr) = mem_op(left, &ctx, ReferenceToken::Bun, "BUN")?;
                    left = nl;
                    tokens.push(instr.label(lab.take()));
                }

                Terminal::Bsa => {
                    let (nl, instr) = mem_op(left, &ctx, ReferenceToken::Bsa, "BSA")?;
                    left = nl;
                    tokens.push(instr.label(lab.take()));
                }

                Terminal::Isz => {
                    let (nl, instr) = mem_op(left, &ctx, ReferenceToken::Isz, "ISZ")?;
                    left = nl;
                    tokens.push(instr.label(lab.take()));
                }

                // Register Ops
                Terminal::Cla => {
                    let (nl, instr) = reg_op(left, &ctx, ReferenceToken::Cla, "CLA")?;
                    left = nl;
                    tokens.push(instr.label(lab.take()));
                }

                Terminal::Cle => {
                    let (nl, instr) = reg_op(left, &ctx, ReferenceToken::Cle, "CLE")?;
                    left = nl;
                    tokens.push(instr.label(lab.take()));
                }

                Terminal::Cma => {
                    let (nl, instr) = reg_op(left, &ctx, ReferenceToken::Cma, "CMA")?;
                    left = nl;
                    tokens.push(instr.label(lab.take()));
                }

                Terminal::Cme => {
                    let (nl, instr) = reg_op(left, &ctx, ReferenceToken::Cme, "CME")?;
                    left = nl;
                    tokens.push(instr.label(lab.take()));
                }

                Terminal::Cir => {
                    let (nl, instr) = reg_op(left, &ctx, ReferenceToken::Cir, "CIR")?;
                    left = nl;
                    tokens.push(instr.label(lab.take()));
                }

                Terminal::Cil => {
                    let (nl, instr) = reg_op(left, &ctx, ReferenceToken::Cil, "CIL")?;
                    left = nl;
                    tokens.push(instr.label(lab.take()));
                }

                Terminal::Inc => {
                    let (nl, instr) = reg_op(left, &ctx, ReferenceToken::Inc, "INC")?;
                    left = nl;
                    tokens.push(instr.label(lab.take()));
                }

                Terminal::Spa => {
                    let (nl, instr) = reg_op(left, &ctx, ReferenceToken::Spa, "SPA")?;
                    left = nl;
                    tokens.push(instr.label(lab.take()));
                }

                Terminal::Sna => {
                    let (nl, instr) = reg_op(left, &ctx, ReferenceToken::Sna, "SNA")?;
                    left = nl;
                    tokens.push(instr.label(lab.take()));
                }

                Terminal::Sze => {
                    let (nl, instr) = reg_op(left, &ctx, ReferenceToken::Sze, "SZE")?;
                    left = nl;
                    tokens.push(instr.label(lab.take()));
                }

                Terminal::Hlt => {
                    let (nl, instr) = reg_op(left, &ctx, ReferenceToken::Hlt, "HLT")?;
                    left = nl;
                    tokens.push(instr.label(lab.take()));
                }

                // IO Ops
                Terminal::Inp => {
                    let (nl, instr) = reg_op(left, &ctx, ReferenceToken::Inp, "INP")?;
                    left = nl;
                    tokens.push(instr.label(lab.take()));
                }

                Terminal::Out => {
                    let (nl, instr) = reg_op(left, &ctx, ReferenceToken::Out, "OUT")?;
                    left = nl;
                    tokens.push(instr.label(lab.take()));
                }

                Terminal::Ski => {
                    let (nl, instr) = reg_op(left, &ctx, ReferenceToken::Ski, "SKI")?;
                    left = nl;
                    tokens.push(instr.label(lab.take()));
                }

                Terminal::Sko => {
                    let (nl, instr) = reg_op(left, &ctx, ReferenceToken::Sko, "SKO")?;
                    left = nl;
                    tokens.push(instr.label(lab.take()));
                }

                Terminal::Ion => {
                    let (nl, instr) = reg_op(left, &ctx, ReferenceToken::Ion, "ION")?;
                    left = nl;
                    tokens.push(instr.label(lab.take()));
                }

                Terminal::Iof => {
                    let (nl, instr) = reg_op(left, &ctx, ReferenceToken::Iof, "IOF")?;
                    left = nl;
                    tokens.push(instr.label(lab.take()));
                }

                // Directives
                Terminal::Org => {
                    if left[1].tval != LTokenVal::NonTerminal {
                        return Err(ParseError {
                            ctx,
                            span: left[0].span,
                            ty: ParseErrorType::DirectiveLiteralMissing,
                        })
                        .map_err(AssembleError::from);
                    }

                    if let Ok(v) = u16::from_str_radix(left[1].span.slice(&ctx.instr), 16) {
                        let instr = ReferenceInstruction {
                            span: left[0].span.join(left[1].span),
                            instr: ReferenceToken::Org(v),
                        };

                        left = eat_nl_com(&left[2..], &ctx, left[0].span.join(left[1].span))?;
                        tokens.push(instr);
                    } else {
                        return Err(ParseError {
                            ctx,
                            span: left[0].span.join(left[1].span),
                            ty: ParseErrorType::LiteralHexValueFormat(left[1].span),
                        })
                        .map_err(AssembleError::from);
                    }
                }

                Terminal::Hex => {
                    if left[1].tval != LTokenVal::NonTerminal {
                        return Err(ParseError {
                            ctx,
                            span: left[0].span,
                            ty: ParseErrorType::DirectiveLiteralMissing,
                        })
                        .map_err(AssembleError::from);
                    }

                    if let Ok(v) = u16::from_str_radix(left[1].span.slice(&ctx.instr), 16) {
                        let instr = ReferenceInstruction {
                            span: left[0].span.join(left[1].span),
                            instr: ReferenceToken::Hex(v),
                        };

                        left = eat_nl_com(&left[2..], &ctx, left[0].span.join(left[1].span))?;
                        tokens.push(instr.label(lab.take()));
                    } else {
                        return Err(ParseError {
                            ctx,
                            span: left[0].span.join(left[1].span),
                            ty: ParseErrorType::LiteralHexValueFormat(left[1].span),
                        })
                        .map_err(AssembleError::from);
                    }
                }

                Terminal::Dec => {
                    if left[1].tval != LTokenVal::NonTerminal {
                        return Err(ParseError {
                            ctx,
                            span: left[0].span,
                            ty: ParseErrorType::DirectiveLiteralMissing,
                        })
                        .map_err(AssembleError::from);
                    }

                    if let Ok(v) = i16::from_str_radix(left[1].span.slice(&ctx.instr), 16) {
                        let instr = ReferenceInstruction {
                            span: left[0].span.join(left[1].span),
                            instr: ReferenceToken::Dec(v),
                        };

                        left = eat_nl_com(&left[2..], &ctx, left[0].span.join(left[1].span))?;
                        tokens.push(instr.label(lab.take()));
                    } else {
                        return Err(ParseError {
                            ctx,
                            span: left[0].span.join(left[1].span),
                            ty: ParseErrorType::LiteralDecValueFormat(left[1].span),
                        })
                        .map_err(AssembleError::from);
                    }
                }

                // Much newlines yum yum yum
                Terminal::Newline => {
                    left = eat_nl_com(&left[..], &ctx, left[0].span)?;
                }
                Terminal::Comma => {
                    return Err(ParseError {
                        ctx,
                        span: left[0].span,
                        ty: ParseErrorType::BareComma,
                    })
                    .map_err(AssembleError::from)
                }
                Terminal::CommentStart => {
                    left = eat_nl_com(&left[..], &ctx, left[0].span)?;
                }
                Terminal::Indirection => {
                    return Err(ParseError {
                        ctx,
                        span: left[0].span,
                        ty: ParseErrorType::BareIndirection,
                    })
                    .map_err(AssembleError::from)
                }
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
