use std::collections::HashMap;

use console::StyledObject;
use thiserror::Error;

pub mod lex;
pub mod parse;

pub use lex::lex;
pub use parse::parse;

#[derive(Error, Debug)]
pub enum AssembleError<'a> {
    #[error("Span could not be created because {0} was after {1}")]
    SpanError(usize, usize),

    #[error("{0}")]
    ParseError(parse::ParseError<'a>),
}

impl<'c, 'a> From<parse::ParseError<'a>> for AssembleError<'a> {
    fn from(pe: parse::ParseError<'a>) -> Self {
        Self::ParseError(pe)
    }
}

pub type Result<'a, T> = std::result::Result<T, AssembleError<'a>>;

#[derive(Debug, Clone, Copy)]
pub struct Span {
    line: Option<usize>,
    char_st: usize,
    char_en: usize,
}

impl Span {
    pub fn new_unchecked(char_st: usize, char_en: usize) -> Self {
        Self {
            line: None,
            char_st,
            char_en,
        }
    }

    pub fn new(char_st: usize, char_en: usize) -> Result<'static, Self> {
        if char_en <= char_st {
            Err(AssembleError::SpanError(char_st, char_en))
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

    pub fn slice<'a>(&self, instr: &'a str) -> &'a str {
        &instr[self.char_st..self.char_en]
    }

    pub fn red<'a>(&'_ self, instr: &'a str) -> StyledObject<&'a str> {
        console::style(&instr[self.char_st..self.char_en]).red()
    }

    /// Prints the span specified by l with the self highlighted in red
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
