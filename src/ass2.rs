#[derive(Debug)]
pub enum AssembleError {
    SpanError(usize, usize),
}

#[derive(Debug)]
pub struct Span {
    line: Option<usize>,
    char_st: usize,
    char_en: usize,
}

pub type Result<T> = std::result::Result<T, AssembleError>;

impl Span {
    pub fn new_unchecked(char_st: usize, char_en: usize) -> Self {
        Self {
            line: None,
            char_st,
            char_en,
        }
    }

    pub fn new(char_st: usize, char_en: usize) -> Result<Self> {
        if char_en <= char_st {
            Err(AssembleError::SpanError(char_st, char_en))
        } else {
            Ok(Self::new_unchecked(char_st, char_en))
        }
    }

    pub fn join(&self, other: &Self) -> Self {
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

pub mod lex;
