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

impl<'a> From<parse::ParseError<'a>> for AssembleError<'a> {
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
        if self.char_st < other.char_st {
            Self {
                line: self.line,
                char_st: self.char_st,
                char_en: other.char_en,
            }
        } else {
            Self {
                line: other.line,
                char_st: other.char_st,
                char_en: self.char_en,
            }
        }
    }

    pub fn line(self, line: usize) -> Self {
        Self {
            line: Some(line),
            ..self
        }
    }

    pub fn slice<'a>(&self, instr: &'a str) -> &'a str {
        &instr[self.char_st..self.char_en]
    }

    pub fn red<'a>(&'_ self, instr: &'a str) -> StyledObject<&'a str> {
        console::style(&instr[self.char_st..self.char_en]).red()
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
