//! Assemble a Mano Program
use std::{cmp::Ordering, collections::HashMap, fmt::Display};

use console::{style, StyledObject};
use log::{debug, info};

pub mod debug;
pub mod layout;
pub mod lex;
pub mod parse;
pub mod release;
pub mod resolve;

pub use debug::debug;
pub use layout::layout;
pub use lex::lex;
pub use parse::parse;
pub use release::release;
pub use resolve::resolve;

use crate::either::*;

pub const MAGIC_VAL: &'static [u8; 4] = b"rano";
const HEADER_LEN: usize = 8;

/// Represents how the reset vector will be chosen,
pub enum ResetVector {
    /// Set the reset vector to the final location of this label
    Label(String),

    /// Set the reset vector to this location
    Location(u16),

    /// Do not set a reset vector
    None,
}

#[derive(Debug)]
/// Internal error creating a span
pub struct SpanError(usize, usize);

impl HeadlineError for SpanError {
    fn headline(&self) -> String {
        format!(
            "Internal: Could not create span [{}, {}) due to {} being before {}",
            self.0, self.1, self.1, self.0
        )
    }

    fn body(&self) -> String {
        String::new()
    }
}

/// Implemented for error types that can be pretty printed
pub trait HeadlineError: std::fmt::Debug {
    /// Not expexted to have any newlines
    fn headline(&self) -> String;

    /// Expected to end with a newline
    fn body(&self) -> String;
}

#[derive(Debug)]
pub enum AssembleError<'a> {
    Span(SpanError),
    Parse(parse::ParseError<'a>),
    Layout(layout::LayoutError<'a>),
    Resolve(resolve::ResolveError<'a>),
}

impl From<SpanError> for AssembleError<'static> {
    fn from(se: SpanError) -> Self {
        AssembleError::Span(se)
    }
}

impl<'a> From<parse::ParseError<'a>> for AssembleError<'a> {
    fn from(pe: parse::ParseError<'a>) -> Self {
        AssembleError::Parse(pe)
    }
}

impl<'a> From<layout::LayoutError<'a>> for AssembleError<'a> {
    fn from(ae: layout::LayoutError<'a>) -> Self {
        AssembleError::Layout(ae)
    }
}

impl<'a> From<resolve::ResolveError<'a>> for AssembleError<'a> {
    fn from(re: resolve::ResolveError<'a>) -> Self {
        AssembleError::Resolve(re)
    }
}

impl<'t> Display for AssembleError<'t> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let headline = match self {
            Self::Span(r) => r.headline(),
            Self::Parse(r) => r.headline(),
            Self::Layout(r) => r.headline(),
            Self::Resolve(r) => r.headline(),
        };

        let body = match self {
            Self::Span(r) => r.body(),
            Self::Parse(r) => r.body(),
            Self::Layout(r) => r.body(),
            Self::Resolve(r) => r.body(),
        };

        writeln!(
            f,
            "{}",
            format!(
                "{}{}",
                style("ERROR: ").red().bold(),
                style(headline).bold()
            )
        )?;
        write!(f, "{}", body)
    }
}

impl<'t> std::error::Error for AssembleError<'t> {}

/// Convenience type returned by [`debug_build`] and [`release_build`]
pub type Result<'a, T> = std::result::Result<T, AssembleError<'a>>;

#[derive(Debug, Clone, Copy)]
/// Represents a slice of the Mano source from which a token originated
pub struct Span {
    line: Option<usize>,
    char_st: usize,
    char_en: usize,
}

impl Span {
    /// Create a new span, do not check that the bounds are ordered correctly
    pub fn new_unchecked(char_st: usize, char_en: usize) -> Self {
        Self {
            line: None,
            char_st,
            char_en,
        }
    }

    /// Creates a span and checks bounds ordering
    pub fn new(char_st: usize, char_en: usize) -> Result<'static, Self> {
        if char_en <= char_st {
            Err(SpanError(char_st, char_en)).map_err(SpanError::into)
        } else {
            Ok(Self::new_unchecked(char_st, char_en))
        }
    }

    pub fn join(&self, other: Self) -> Self {
        let line = self
            .line
            .map(|a| other.line.map(|b| if a > b { b } else { a }).unwrap_or(a))
            .or(other.line);

        let char_st = if self.char_st < other.char_st {
            self.char_st
        } else {
            other.char_st
        };

        let char_en = if other.char_en < self.char_en {
            self.char_en
        } else {
            other.char_en
        };

        Self {
            line,
            char_st,
            char_en,
        }
    }

    /// Set the line number for this span
    pub fn line(self, line: usize) -> Self {
        Self {
            line: Some(line),
            ..self
        }
    }

    /// True if self is a subregion of other
    pub fn subset_of(&self, other: Self) -> bool {
        other.char_st <= self.char_st && self.char_en <= other.char_en
    }

    /// Get the src text that this span represents
    pub fn slice<'a>(&self, instr: &'a str) -> &'a str {
        &instr[self.char_st..self.char_en]
    }

    /// Highlight the src text that this span represents in red
    pub fn red<'a>(&'_ self, instr: &'a str) -> StyledObject<&'a str> {
        console::style(&instr[self.char_st..self.char_en]).red()
    }

    /// Prints the span specified by l with the self highlighted in red
    ///
    /// self must be a subset of l
    #[deprecated(since = "0.2.0", note = "Please use red_in_lines instead")]
    pub fn red_in<'a>(&'a self, instr: &'a str, l: Span) -> String {
        if !self.subset_of(l) {
            unreachable!("{} is not in {} and cannot be styled as such", l, self);
        }

        format!(
            "{}{}{}",
            &instr[l.char_st..self.char_st],
            console::style(&instr[self.char_st..self.char_en]).red(),
            &instr[self.char_en..l.char_en],
        )
    }

    /// Prints all the lines spanned by self highlighting the region self in red
    ///
    /// Optionally add lines of context on either end
    pub fn red_in_lines(&self, ctx: &ParseContext, lines: usize) -> String {
        SpanSet(vec![*self]).red_ctx(ctx, lines)
    }

    /// Create a span_set containing only this span
    pub fn into_set(self) -> SpanSet {
        SpanSet(vec![self])
    }

    pub fn overlapping(&self, other: &Self) -> bool {
        (self.char_st >= other.char_st && self.char_st <= other.char_en)
            || (self.char_en >= other.char_st && self.char_en <= other.char_en)
    }

    /// Join the two spans only if they abut one another or are overlapping
    pub fn maybe_join(self, other: Self) -> Either<Self, (Self, Self)> {
        if self.overlapping(&other) {
            L(self.join(other))
        } else {
            R((self, other))
        }
    }

    pub fn len(&self) -> usize {
        self.char_en - self.char_st
    }
}

pub struct SpanSet(Vec<Span>);

impl SpanSet {
    /// Create a new span set with 0 capacity
    pub fn new() -> Self {
        Self(Vec::new())
    }

    /// Create a new span set with a predefined capacity
    pub fn with_capacity(capacity: usize) -> Self {
        Self(Vec::with_capacity(capacity))
    }

    /// insert a new span into the set, coalecing with any that overlap
    pub fn insert(&mut self, mut ins: Span) -> &mut Self {
        debug!("Inserted span {}", ins);
        let inner = &mut self.0;

        let mut overlapping = Vec::with_capacity(inner.len());

        for (idx, i) in inner.iter().enumerate() {
            if i.overlapping(&ins) {
                overlapping.push(idx);
                ins = ins.join(*i);
            }
        }

        debug!("Found overlapping {:?}", overlapping);
        for i in overlapping {
            inner.remove(i);
        }

        inner.push(ins);
        inner.sort_by(|a, b| {
            if a.char_st < b.char_st {
                Ordering::Less
            } else {
                Ordering::Greater
            }
        });

        self
    }

    /// Insert a new span into the set without coalecing
    ///
    /// ## Safety:
    ///
    /// The inner data keeps the invariant that all fields of the inner spans are sorted by all
    /// three of their fields by coalecing overlapping spans together. This must be kept by
    /// unchecked insertions into the type.
    pub unsafe fn insert_unchecked(&mut self, ins: Span) -> &mut Self {
        info!("Inserted span {}", ins);
        self.0.push(ins);
        self.0.sort_by(|a, b| {
            if a.char_st < b.char_st {
                Ordering::Less
            } else {
                Ordering::Greater
            }
        });
        self
    }

    /// Highlight all the sections in this SpanSet with c_lines padding above and below
    pub fn red_ctx(&self, ctx: &ParseContext, c_lines: usize) -> String {
        // INVARIANT: all spans are monotonically increasing on all fields and do not overlap
        // whatsovere

        if self.0.len() == 0 {
            return format!("");
        }

        let instr = &ctx.instr;
        let linesmap = &ctx.lines;
        let spans = &self.0;

        let mut start_line = spans[0].line.expect("Missing line for start");
        let mut end_line = spans[spans.len() - 1].line.expect("Missing line for end");

        // Expand
        if c_lines > start_line {
            start_line = 0;
        } else {
            start_line -= c_lines;
        }
        if c_lines >= linesmap.len() - end_line {
            end_line = linesmap.len() - 1;
        } else {
            end_line += c_lines;
        }

        let spans = self.0.as_slice();

        let mut line = start_line;
        let mut linespan = linesmap
            .get(&(line as u32))
            .expect("Found line that was not in context");

        let mut cur = 0;
        let mut curspan = &spans[cur];

        let mut last_b = linespan.char_st;
        //
        // TODO instead call with_capacity using heuristic based on line count// TODO instead call with_capacity using heuristic based on line count
        let mut out = String::new();

        while line <= end_line {
            if last_b == linespan.char_st {
                out.push_str(&format!(" {:3.}  ", line));
            }

            if cur > spans.len() || last_b < curspan.char_st {
                if cur > spans.len() || curspan.char_st > linespan.char_en {
                    // Print to the end of the line and advance the line

                    out.push_str(&format!("{}\n", &instr[last_b..linespan.char_en]));

                    line += 1;
                    linespan = linesmap
                        .get(&(line as u32))
                        .expect("Found line that was not in context");
                    last_b = linespan.char_st;
                } else {
                    // Print to the beginning of the span

                    out.push_str(&instr[last_b..curspan.char_st]);
                    last_b = curspan.char_st;
                }
            } else {
                // inside a span, print red
                if curspan.char_en > linespan.char_en {
                    // Print to the end of line and advance line

                    out.push_str(&format!(
                        "{}\n",
                        style(&instr[last_b..linespan.char_en]).red().to_string()
                    ));

                    line += 1;
                    linespan = linesmap
                        .get(&(line as u32))
                        .expect("Found line that was not in context");
                    last_b = linespan.char_st;
                } else {
                    // Print to end of span and advance span

                    out.push_str(&style(&instr[last_b..curspan.char_en]).red().to_string());
                    last_b = curspan.char_en;

                    cur += 1;
                    if cur < spans.len() {
                        curspan = &spans[cur];
                    }
                }
            }
        }

        out
    }
}

impl std::fmt::Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(line) = self.line {
            write!(
                f,
                "Line: {}, Chars [{}, {})",
                line, self.char_st, self.char_en
            )
        } else {
            write!(f, "Line: ?, Chars [{}, {})", self.char_st, self.char_en)
        }
    }
}

#[derive(Debug, Clone)]
pub struct ParseContext<'a> {
    instr: &'a str,
    lines: HashMap<u32, Span>,
}

/// Assemble a release build of instr. See module `release` for file layout information.
pub fn release_build(instr: &str, reset: ResetVector) -> Result<Box<[u8]>> {
    lex(instr)
        .and_then(parse)
        .and_then(|a| layout(a, reset))
        .and_then(resolve)
        .and_then(release)
}

/// Assemble a debug build of instr. See module `debug` for file layout information.
pub fn debug_build(instr: &str, reset: ResetVector) -> Result<Box<[u8]>> {
    lex(instr)
        .and_then(parse)
        .and_then(|a| layout(a, reset))
        .and_then(resolve)
        .and_then(debug)
}
