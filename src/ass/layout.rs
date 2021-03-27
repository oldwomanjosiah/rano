//! Layout instructions and data in the file

use std::fmt::Display;

use super::*;
use parse::{ReferenceToken, TokenTree};

#[derive(Debug)]
pub enum LayoutErrorType {
    /// Indicates that a reference was defined more than one time in the assembly,
    /// which is illiegal
    ReferenceRedefinition(Span, Span),

    /// Indicates that an org directive would cause some section to overwrite existing instructions
    OrgOverlapping {
        before: Span,
        here: Span,
        before_org: u16,
        before_len: u16,
        here_org: u16,
    },
}

#[derive(Debug)]
pub struct LayoutError<'a> {
    ctx: ParseContext<'a>,
    ty: LayoutErrorType,
}

impl<'a> HeadlineError for LayoutError<'a> {
    fn headline(&self) -> String {
        match self.ty {
            LayoutErrorType::ReferenceRedefinition(s, _) => format!(
                "The label {} may not be defined more than once",
                s.slice(self.ctx.instr)
            ),
            LayoutErrorType::OrgOverlapping { .. } => format!("Two or more hunks overlap!"),
        }
    }

    fn body(&self) -> String {
        match &self.ty {
            LayoutErrorType::ReferenceRedefinition(first, second) => {
                let combine = if let (Some(f), Some(s)) = (first.line, second.line) {
                    let dif = f as isize - s as isize;
                    if -4 < dif && dif < 4 {
                        true
                    } else {
                        false
                    }
                } else {
                    false
                };

                if combine {
                    format!(
                        "The label is defined in at least these places:\n{}",
                        first.into_set().insert(*second).red_ctx(&self.ctx, 2)
                    )
                } else {
                    format!(
                        "It was first defined here:\n{}And then again here:\n{}",
                        first.into_set().red_ctx(&self.ctx, 2),
                        second.into_set().red_ctx(&self.ctx, 2)
                    )
                }
            }

            LayoutErrorType::OrgOverlapping {
                before,
                here,
                before_org: first,
                before_len: len,
                here_org: second,
            } => {
                format!(
                    "The ORG defined here:\n\
                    {}\
                    is {} instructions (words) long \
                    starting at 0x{:03X} and would overlap with the following by {} instructions:\n\
                    {}\
                    You may wish to move the second ORG directive (on line {}) to 0x{:03X}\n",
                    before.into_set().red_ctx(&self.ctx, 2),
                    len,
                    first,
                    *len - (*second - *first),
                    here.into_set().red_ctx(&self.ctx, 2),
                    here.line.unwrap(),
                    first + len,
                )
            }
        }
    }
}

#[derive(Debug)]
pub enum LayoutWarningType {}

#[derive(Debug)]
pub enum PartialInstruction<'a> {
    Unresolved {
        instruction: Span,
        partial: u16,
        unresolved: &'a str,
        at: Span,
    },
    Complete(Span, u16),
}

impl<'a> PartialInstruction<'a> {
    fn memory(
        instruction: Span,
        instr: &'a str,
        reference: Span,
        val: u16,
        imm: bool,
    ) -> PartialInstruction<'a> {
        PartialInstruction::Unresolved {
            instruction,
            partial: mask(imm) | val,
            unresolved: reference.slice(instr),
            at: reference,
        }
    }

    fn register(instruction: Span, num: usize) -> PartialInstruction<'static> {
        debug_assert!(num <= 10, "This would overflow the instruction");
        PartialInstruction::Complete(instruction, 0x7000 | (0x0800 >> num))
    }

    fn io(instruction: Span, num: usize) -> PartialInstruction<'static> {
        debug_assert!(num <= 10, "This would overflow the instruction");
        PartialInstruction::Complete(instruction, 0xF000 | (0x0800 >> num))
    }
}

#[derive(Debug)]
pub struct Hunk<'a> {
    pub org: u16,
    pub extent: u16,
    pub instructions: Box<[PartialInstruction<'a>]>,

    /// The org directive location
    pub at: Span,
}

#[derive(Debug)]
pub struct Layout<'a> {
    pub ctx: ParseContext<'a>,
    pub references: HashMap<&'a str, (u16, Span)>,
    pub hunks: Box<[Hunk<'a>]>,
}

fn mask(b: bool) -> u16 {
    (b as u16) << 15
}

const HUNK_LEN_HEUR: usize = 12;
const REF_P_HUNK_HEUR: usize = 2;

/// Layout instructions to their final addresses in hunks and build a list of known references
pub fn layout(TokenTree { ctx, tokens }: TokenTree) -> Result<Layout> {
    let mut hunks = Vec::with_capacity(tokens.len() / HUNK_LEN_HEUR);
    let mut cur_hunk = Vec::with_capacity(HUNK_LEN_HEUR);
    let mut references = HashMap::with_capacity(tokens.len() / HUNK_LEN_HEUR * REF_P_HUNK_HEUR);

    let mut last_org = 0u16;
    let mut last_org_span = Span::new_unchecked(0, 0);

    info!(
        "Starting layout step with a Hunk Length Heur of {} and a ref per hunk heur of {}",
        HUNK_LEN_HEUR, REF_P_HUNK_HEUR
    );

    let mut instrs = tokens.iter();
    let mut next = None;

    loop {
        let inext = match next
            .take()
            .map(|a| Some(a))
            .unwrap_or_else(|| instrs.next())
        {
            Some(a) => a,
            None => break,
        };

        match &inext.instr {
            ReferenceToken::And(reference, imm) => {
                cur_hunk.push(PartialInstruction::memory(
                    inext.span, &ctx.instr, *reference, 0x0000, *imm,
                ));
            }
            ReferenceToken::Add(reference, imm) => {
                cur_hunk.push(PartialInstruction::memory(
                    inext.span, &ctx.instr, *reference, 0x1000, *imm,
                ));
            }
            ReferenceToken::Lda(reference, imm) => {
                cur_hunk.push(PartialInstruction::memory(
                    inext.span, &ctx.instr, *reference, 0x2000, *imm,
                ));
            }
            ReferenceToken::Sta(reference, imm) => {
                cur_hunk.push(PartialInstruction::memory(
                    inext.span, &ctx.instr, *reference, 0x3000, *imm,
                ));
            }
            ReferenceToken::Bun(reference, imm) => {
                cur_hunk.push(PartialInstruction::memory(
                    inext.span, &ctx.instr, *reference, 0x4000, *imm,
                ));
            }
            ReferenceToken::Bsa(reference, imm) => {
                cur_hunk.push(PartialInstruction::memory(
                    inext.span, &ctx.instr, *reference, 0x5000, *imm,
                ));
            }
            ReferenceToken::Isz(reference, imm) => {
                cur_hunk.push(PartialInstruction::memory(
                    inext.span, &ctx.instr, *reference, 0x6000, *imm,
                ));
            }

            // Register Ops
            ReferenceToken::Cla() => cur_hunk.push(PartialInstruction::register(inext.span, 0)),
            ReferenceToken::Cle() => cur_hunk.push(PartialInstruction::register(inext.span, 1)),
            ReferenceToken::Cma() => cur_hunk.push(PartialInstruction::register(inext.span, 2)),
            ReferenceToken::Cme() => cur_hunk.push(PartialInstruction::register(inext.span, 3)),
            ReferenceToken::Cir() => cur_hunk.push(PartialInstruction::register(inext.span, 4)),
            ReferenceToken::Cil() => cur_hunk.push(PartialInstruction::register(inext.span, 5)),
            ReferenceToken::Inc() => cur_hunk.push(PartialInstruction::register(inext.span, 6)),
            ReferenceToken::Spa() => cur_hunk.push(PartialInstruction::register(inext.span, 7)),
            ReferenceToken::Sna() => cur_hunk.push(PartialInstruction::register(inext.span, 8)),
            ReferenceToken::Sze() => cur_hunk.push(PartialInstruction::register(inext.span, 9)),
            ReferenceToken::Hlt() => cur_hunk.push(PartialInstruction::register(inext.span, 10)),

            // IO Ops
            ReferenceToken::Inp() => cur_hunk.push(PartialInstruction::io(inext.span, 0)),
            ReferenceToken::Out() => cur_hunk.push(PartialInstruction::io(inext.span, 1)),
            ReferenceToken::Ski() => cur_hunk.push(PartialInstruction::io(inext.span, 2)),
            ReferenceToken::Sko() => cur_hunk.push(PartialInstruction::io(inext.span, 3)),
            ReferenceToken::Ion() => cur_hunk.push(PartialInstruction::io(inext.span, 4)),
            ReferenceToken::Iof() => cur_hunk.push(PartialInstruction::io(inext.span, 5)),

            // Direct values
            ReferenceToken::Hex(v) => cur_hunk.push(PartialInstruction::Complete(inext.span, *v)),
            ReferenceToken::Dec(v) => {
                cur_hunk.push(PartialInstruction::Complete(inext.span, *v as u16))
            }

            ReferenceToken::LabelDef(span, ins) => {
                next = Some(ins.as_ref());
                match references.insert(
                    span.slice(&ctx.instr),
                    (last_org + cur_hunk.len() as u16, *span),
                ) {
                    None => (),
                    Some((_, last)) => {
                        return Err(LayoutError {
                            ctx,
                            ty: LayoutErrorType::ReferenceRedefinition(last, *span),
                        })
                        .map_err(LayoutError::into);
                    }
                }
            }

            ReferenceToken::Org(new_org) => {
                if cur_hunk.len() > 0 {
                    cur_hunk.shrink_to_fit();
                    hunks.push(Hunk {
                        org: last_org,
                        extent: cur_hunk.len() as u16,
                        instructions: cur_hunk.into_boxed_slice(),
                        at: last_org_span,
                    });

                    cur_hunk = Vec::with_capacity(HUNK_LEN_HEUR);
                }
                last_org = *new_org;
                last_org_span = inext.span;
            }
        }
    }

    let optimal_hunk_heur = hunks.len() as f32 / tokens.len() as f32;
    info!(
        "Best Hunk len heur for thsi run would have been {:.2}",
        optimal_hunk_heur
    );

    info!(
        "Original hunks cap: {}, Final hunks cap: {}, grew by: {}, wasted cap: {}",
        tokens.len() / HUNK_LEN_HEUR,
        hunks.capacity(),
        hunks.capacity() - tokens.len() / HUNK_LEN_HEUR,
        hunks.capacity() - hunks.len()
    );

    info!(
        "Best ref per hunk heur for this run would have been {:.2}",
        references.len() as f32 * optimal_hunk_heur / tokens.len() as f32
    );

    info!(
        "Original references cap: {}, final cap: {}, grew by: {}, wasted cap: {}",
        tokens.len() / HUNK_LEN_HEUR * REF_P_HUNK_HEUR,
        references.capacity(),
        references.capacity() - (tokens.len() / HUNK_LEN_HEUR * REF_P_HUNK_HEUR),
        references.capacity() - references.len()
    );

    // Check if any hunks are overlapping
    hunks.sort_by(|s1, s2| {
        if s1.org < s2.org {
            Ordering::Less
        } else {
            Ordering::Greater
        }
    });
    let hunks = hunks.into_boxed_slice();

    for i in 0..hunks.len() - 1 {
        let cur = &hunks[i];
        let next = &hunks[i + 1];

        if cur.org + cur.extent > next.org {
            return Err(LayoutError {
                ctx,
                ty: LayoutErrorType::OrgOverlapping {
                    before: cur.at,
                    here: next.at,
                    before_org: cur.org,
                    before_len: cur.extent,
                    here_org: next.org,
                },
            })
            .map_err(LayoutError::into);
        }
    }

    Ok(Layout {
        ctx,
        references,
        hunks,
    })
}
