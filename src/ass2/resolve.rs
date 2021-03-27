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

impl<'a> Display for ResolveError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.ty {
            ResolveErrorType::UnresolvedReference(s) => {
                writeln!(f, "Could not resolve reference")?;
                write!(f, "{}", s.into_set().red_ctx(&self.ctx, 2))
            }
        }
    }
}

#[derive(Debug)]
pub struct Hunk {
    org: u16,
    extent: u16,
    instructions: Box<[(u16, Span)]>,

    /// The org directive location
    at: Span,
}

#[derive(Debug)]
pub struct Resolved<'a> {
    ctx: ParseContext<'a>,
    references: HashMap<&'a str, (u16, Span)>,
    hunks: Box<[Hunk]>,
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
