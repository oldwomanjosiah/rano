use std::fmt::Display;

use super::*;
use layout::Hunk as LHunk;
use layout::Layout;
use layout::PartialInstruction;

#[derive(Debug)]
pub enum ResolveErrorType {
    UnresolvedReference(Span),
}

#[derive(Debug)]
pub struct ResolveError<'a> {
    ctx: ParseContext<'a>,
    ty: ResolveErrorType,
}

impl<'a> HeadlineError for ResolveError<'a> {
    fn headline(&self) -> String {
        match self.ty {
            ResolveErrorType::UnresolvedReference(_) => format!("Could not resolve reference"),
        }
    }

    fn body(&self) -> String {
        match self.ty {
            ResolveErrorType::UnresolvedReference(s) => {
                format!("{}", s.into_set().red_ctx(&self.ctx, 2))
            }
        }
    }
}

#[derive(Debug)]
pub struct Hunk {
    pub org: u16,
    pub extent: u16,
    pub instructions: Box<[(u16, Span)]>,

    /// The org directive location
    pub at: Span,
}

#[derive(Debug)]
pub struct Resolved<'a> {
    pub ctx: ParseContext<'a>,
    pub references: HashMap<&'a str, (u16, Span)>,
    pub hunks: Box<[Hunk]>,
}

pub fn resolve(
    Layout {
        ctx,
        references,
        hunks,
    }: Layout,
) -> Result<Resolved> {
    info!("Starting resolve step");

    let mut ohunks = Vec::with_capacity(hunks.len());

    for hunk in hunks.into_iter() {
        let mut ohunk = Vec::with_capacity(hunk.instructions.len());

        for instr in hunk.instructions.into_iter() {
            match instr {
                &PartialInstruction::Complete(s, v) => ohunk.push((v, s)),
                &PartialInstruction::Unresolved {
                    instruction,
                    partial,
                    unresolved,
                    at,
                } => {
                    if let Some(location) = references.get(unresolved) {
                        ohunk.push((partial | (0x0FFF & location.0), instruction));
                    } else {
                        return Err(ResolveError {
                            ctx,
                            ty: ResolveErrorType::UnresolvedReference(at),
                        })
                        .map_err(ResolveError::into);
                    }
                }
            }
        }

        ohunks.push(Hunk {
            org: hunk.org,
            extent: hunk.extent,
            instructions: ohunk.into_boxed_slice(),
            at: hunk.at,
        });
    }

    info!("Finished resolve step");

    Ok(Resolved {
        ctx,
        references,
        hunks: ohunks.into_boxed_slice(),
    })
}
