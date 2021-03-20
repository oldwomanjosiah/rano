//! Assemble mano program text into a stripped binary

use std::{collections::HashMap, fmt::Debug, ops::Deref, rc::Rc};

extern crate console;
extern crate log;
extern crate nom;
extern crate thiserror;

use console::Style;
use log::info;
use log::{error, warn};
use thiserror::Error;

pub const CURRENT_VERSION: (u8, u8) = (1, 0);
pub const HEADER_SIZE: usize = 16;

// {{{ Spans

#[derive(Debug, Clone)]
enum Span {
    Section(Rc<SectionSpan>),
    Token(Rc<TokenSpan>),
}

impl From<SectionSpan> for Span {
    fn from(sec: SectionSpan) -> Self {
        Self::Section(Rc::new(sec))
    }
}

impl From<Rc<SectionSpan>> for Span {
    fn from(sec: Rc<SectionSpan>) -> Self {
        Self::Section(sec.clone())
    }
}

impl From<TokenSpan> for Span {
    fn from(tok: TokenSpan) -> Self {
        Self::Token(Rc::new(tok))
    }
}

impl From<Rc<TokenSpan>> for Span {
    fn from(tok: Rc<TokenSpan>) -> Self {
        Self::Token(tok.clone())
    }
}

impl Span {
    pub fn error(&self, instr: &str) -> String {
        match self {
            Self::Section(sec) => sec.error(instr),
            Self::Token(tok) => tok.error(instr),
        }
    }

    pub fn line(&self) -> usize {
        match self {
            Self::Section(sec) => sec.l_idx,
            Self::Token(tok) => tok.section.l_idx,
        }
    }
}

#[derive(Debug, Clone)]
struct SectionSpan {
    l_idx: usize,
    l_bs: (usize, usize),
    s_bs: (usize, usize),
}

impl SectionSpan {
    pub fn join(&self, other: impl AsRef<Self>) -> SectionSpan {
        let other = other.as_ref();

        SectionSpan {
            l_idx: self.l_idx,
            l_bs: (self.l_bs.0, other.l_bs.1),
            s_bs: (self.s_bs.0, other.s_bs.1),
        }
    }

    pub fn apply_style(&self, instr: &str, style: Style) -> String {
        format!(
            "{}{}{}",
            &instr[self.l_bs.0..self.s_bs.0],
            style.apply_to(&instr[self.s_bs.0..self.s_bs.1]),
            &instr[self.s_bs.1..self.l_bs.1]
        )
    }

    pub fn underline(&self, instr: &str) -> String {
        let style = console::Style::new().underlined();

        self.apply_style(instr, style)
    }

    pub fn red(&self, instr: &str) -> String {
        let style = console::Style::new().red();

        self.apply_style(instr, style)
    }

    pub fn error(&self, instr: &str) -> String {
        self.red(instr)
    }
}

#[derive(Debug, Clone)]
struct TokenSpan {
    section: Rc<SectionSpan>,
    t_bs: (usize, usize),
}

impl TokenSpan {
    pub fn join(&self, other: impl AsRef<Self>) -> TokenSpan {
        let other = other.as_ref();

        TokenSpan {
            section: Rc::new(self.section.join(other.section.clone())),
            t_bs: (self.t_bs.0, other.t_bs.1),
        }
    }

    pub fn apply_style(&self, instr: &str, style: Style) -> String {
        format!(
            "{}{}{}",
            &instr[self.section.l_bs.0..self.t_bs.0],
            style.apply_to(&instr[self.t_bs.0..self.t_bs.1]),
            &instr[self.t_bs.1..self.section.l_bs.1]
        )
    }

    pub fn underline(&self, instr: &str) -> String {
        let style = console::Style::new().underlined();

        self.apply_style(instr, style)
    }

    pub fn red(&self, instr: &str) -> String {
        let style = console::Style::new().red();

        self.apply_style(instr, style)
    }

    pub fn error(&self, instr: &str) -> String {
        self.red(instr)
    }
}

// }}}

#[derive(Debug)]
enum Instruction<Reference: Debug> {
    // Memory Arithmetic Instructions
    AND(Reference),
    ANDI(Reference),

    ADD(Reference),
    ADDI(Reference),

    LDA(Reference),
    LDAI(Reference),

    STA(Reference),
    STAI(Reference),

    BUN(Reference),
    BUNI(Reference),

    BSA(Reference),
    BSAI(Reference),

    ISZ(Reference),
    ISZI(Reference),

    // Register Instructions
    CLA,
    CLE,
    CMA,
    CME,
    CIR,
    CIL,
    INC,
    SPA,
    SNA,
    SZA,
    SZE,
    HLT,

    // IO Instructions
    INP,
    OUT,
    SKI,
    SKO,
    ION,
    IOF,
}

impl<T: Debug> Instruction<T> {
    pub fn map<U: Debug>(self, f: impl Fn(T) -> U) -> Instruction<U> {
        match self {
            Instruction::AND(t) => Instruction::AND(f(t)),
            Instruction::ANDI(t) => Instruction::ANDI(f(t)),

            Instruction::ADD(t) => Instruction::ADD(f(t)),
            Instruction::ADDI(t) => Instruction::ADDI(f(t)),

            Instruction::LDA(t) => Instruction::LDA(f(t)),
            Instruction::LDAI(t) => Instruction::LDAI(f(t)),

            Instruction::STA(t) => Instruction::STA(f(t)),
            Instruction::STAI(t) => Instruction::STAI(f(t)),

            Instruction::BUN(t) => Instruction::BUN(f(t)),
            Instruction::BUNI(t) => Instruction::BUNI(f(t)),

            Instruction::BSA(t) => Instruction::BSA(f(t)),
            Instruction::BSAI(t) => Instruction::BSAI(f(t)),

            Instruction::ISZ(t) => Instruction::ISZ(f(t)),
            Instruction::ISZI(t) => Instruction::ISZI(f(t)),

            Instruction::CLA => Instruction::CLA,
            Instruction::CLE => Instruction::CLE,
            Instruction::CMA => Instruction::CMA,
            Instruction::CME => Instruction::CME,
            Instruction::CIR => Instruction::CIR,
            Instruction::CIL => Instruction::CIL,
            Instruction::INC => Instruction::INC,
            Instruction::SPA => Instruction::SPA,
            Instruction::SNA => Instruction::SNA,
            Instruction::SZA => Instruction::SZA,
            Instruction::SZE => Instruction::SZE,
            Instruction::HLT => Instruction::HLT,

            Instruction::INP => Instruction::INP,
            Instruction::OUT => Instruction::OUT,
            Instruction::SKI => Instruction::SKI,
            Instruction::SKO => Instruction::SKO,
            Instruction::ION => Instruction::ION,
            Instruction::IOF => Instruction::IOF,
        }
    }

    pub fn try_map<U: Debug, E: std::error::Error>(
        self,
        f: impl Fn(T) -> Result<U, E>,
    ) -> Result<Instruction<U>, E> {
        match self.map(f) {
            Instruction::AND(a) => Ok(Instruction::AND(a?)),

            Instruction::ANDI(a) => Ok(Instruction::ANDI(a?)),

            Instruction::ADD(a) => Ok(Instruction::ADD(a?)),

            Instruction::ADDI(a) => Ok(Instruction::ADDI(a?)),

            Instruction::LDA(a) => Ok(Instruction::LDA(a?)),

            Instruction::LDAI(a) => Ok(Instruction::LDAI(a?)),

            Instruction::STA(a) => Ok(Instruction::STA(a?)),

            Instruction::STAI(a) => Ok(Instruction::STAI(a?)),

            Instruction::BUN(a) => Ok(Instruction::BUN(a?)),

            Instruction::BUNI(a) => Ok(Instruction::BUNI(a?)),

            Instruction::BSA(a) => Ok(Instruction::BSA(a?)),

            Instruction::BSAI(a) => Ok(Instruction::BSAI(a?)),

            Instruction::ISZ(a) => Ok(Instruction::ISZ(a?)),

            Instruction::ISZI(a) => Ok(Instruction::ISZI(a?)),

            Instruction::CLA => Ok(Instruction::CLA),
            Instruction::CLE => Ok(Instruction::CLE),
            Instruction::CMA => Ok(Instruction::CMA),
            Instruction::CME => Ok(Instruction::CME),
            Instruction::CIR => Ok(Instruction::CIR),
            Instruction::CIL => Ok(Instruction::CIL),
            Instruction::INC => Ok(Instruction::INC),
            Instruction::SPA => Ok(Instruction::SPA),
            Instruction::SNA => Ok(Instruction::SNA),
            Instruction::SZA => Ok(Instruction::SZA),
            Instruction::SZE => Ok(Instruction::SZE),
            Instruction::HLT => Ok(Instruction::HLT),

            Instruction::INP => Ok(Instruction::INP),
            Instruction::OUT => Ok(Instruction::OUT),
            Instruction::SKI => Ok(Instruction::SKI),
            Instruction::SKO => Ok(Instruction::SKO),
            Instruction::ION => Ok(Instruction::ION),
            Instruction::IOF => Ok(Instruction::IOF),
        }
    }
}

impl Instruction<u16> {
    fn as_u16(&self) -> u16 {
        match self {
            Instruction::AND(v) => 0x0000 | (0x0FFF & v),
            Instruction::ANDI(v) => 0x8000 | (0x0FFF & v),

            Instruction::ADD(v) => 0x1000 | (0x0FFF & v),
            Instruction::ADDI(v) => 0x9000 | (0x0FFF & v),

            Instruction::LDA(v) => 0x2000 | (0x0FFF & v),
            Instruction::LDAI(v) => 0xA000 | (0x0FFF & v),

            Instruction::STA(v) => 0x3000 | (0x0FFF & v),
            Instruction::STAI(v) => 0xB000 | (0x0FFF & v),

            Instruction::BUN(v) => 0x4000 | (0x0FFF & v),
            Instruction::BUNI(v) => 0xC000 | (0x0FFF & v),

            Instruction::BSA(v) => 0x5000 | (0x0FFF & v),
            Instruction::BSAI(v) => 0xD000 | (0x0FFF & v),

            Instruction::ISZ(v) => 0x6000 | (0x0FFF & v),
            Instruction::ISZI(v) => 0xE000 | (0x0FFF & v),

            Instruction::CLA => 0x7800,
            Instruction::CLE => 0x7400,
            Instruction::CMA => 0x7200,
            Instruction::CME => 0x7100,
            Instruction::CIR => 0x7080,
            Instruction::CIL => 0x7040,
            Instruction::INC => 0x7020,
            Instruction::SPA => 0x7010,
            Instruction::SNA => 0x7008,
            Instruction::SZA => 0x7004,
            Instruction::SZE => 0x7002,
            Instruction::HLT => 0x7001,

            Instruction::INP => 0xF800,
            Instruction::OUT => 0xF400,
            Instruction::SKI => 0xF200,
            Instruction::SKO => 0xF100,
            Instruction::ION => 0xF080,
            Instruction::IOF => 0xF040,
        }
    }
}

#[derive(Debug)]
enum Directive<Reference: Debug> {
    ORG(u16),
    Label(Reference),
    HEX(u16),
    DEC(i16),
}

impl Directive<u16> {
    fn as_u16(&self) -> u16 {
        match self {
            Directive::ORG(_) => unreachable!("Org directive must be stripped before assmbling"),
            Directive::HEX(h) => *h,
            Directive::DEC(d) => *d as u16,
            Directive::Label(_) => {
                unreachable!("Label Directives must be stripped before assembling")
            }
        }
    }
}

#[derive(Debug)]
enum Line<Reference: Debug> {
    Instruction {
        span: Rc<SectionSpan>,
        addr: Option<u16>,
        instr: Instruction<Reference>,
    },
    Directive {
        span: Rc<SectionSpan>,
        addr: Option<u16>,
        dir: Directive<Reference>,
    },
}

impl Line<u16> {
    fn addr(&self) -> Option<u16> {
        match self {
            Line::Instruction {
                span: _,
                addr,
                instr: _,
            } => addr.clone(),
            Line::Directive {
                span: _,
                addr,
                dir: _,
            } => addr.clone(),
        }
    }

    fn as_u16(&self) -> u16 {
        match self {
            Line::Instruction {
                span: _,
                addr: _,
                instr,
            } => instr.as_u16(),
            Line::Directive {
                span: _,
                addr: _,
                dir,
            } => dir.as_u16(),
        }
    }
}

#[derive(Debug, Error)]
pub enum AssembleErrorType<'a> {
    #[error("No Instruction or directive {0}")]
    /// No instruction or directive
    InvalidInstruction(&'a str),

    #[error("Instruction {0} does not take indirection flag `I`")]
    /// Instruction does not take indirection flag
    InvalidIndirectionArgument(&'a str),

    #[error("Expected indirection flag `I` found {0}")]
    /// Expected indirection flag but found x
    ExpectedIndirectionArgument(&'a str),

    #[error("Malformed Instruction: Expected {0}-{1} arguements but found {2}")]
    /// Expected number of tokens but found n
    MalformedInstruction(usize, usize, usize),

    #[error("Malformed Instruction {0}: Expected {1}-{2} arguements but found {3}")]
    /// Instruction format expected arguments but found n
    MalformedArguments(&'a str, usize, usize, usize),

    #[error("Instruction {0} did not expect any argument, found {1}")]
    /// Instruction does not take arguements
    NoArgumentsExpected(&'a str, usize),

    #[error("Could not parse literal value:\n{0}")]
    /// Could not parse literal value
    LiteralParseError(&'a str),

    #[error("Cannot move origin backwards from {0} to {1}")]
    /// Org directive tried to move backwards
    InvalidOrgDirective(usize, usize),

    #[error("Cannot have two labels in a row: {0} and {1}")]
    /// Tried to have 2 or more labels in a row
    DoubleLabelError(&'a str, &'a str),

    #[error("Could not find definition for {0}")]
    /// Could not find reference definition for n
    MissingReference(&'a str),
}

#[derive(Debug)]
pub struct AssembleError<'a> {
    instr: &'a str,
    span: Span,
    message: AssembleErrorType<'a>,
}

impl<'a> std::fmt::Display for AssembleError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}", self.message)?;
        writeln!(f, "On Line: {}", self.span.line() + 1)?;
        writeln!(f, "{}", self.span.error(self.instr))
    }
}

impl<'a> std::error::Error for AssembleError<'a> {}

#[derive(Debug)]
struct Section {
    span: Rc<SectionSpan>,
    token: Vec<Rc<TokenSpan>>,
}

#[derive(Debug)]
struct PreLexed<'a> {
    instr: &'a str,
    sections: Vec<Section>,
}

// {{{ pre_lex
fn pre_lex<'a>(instr: &'a str) -> Result<PreLexed<'a>, AssembleError<'a>> {
    info!("Beggining pre-lex");

    let lines = {
        let mut start = 0;

        instr.lines().enumerate().map(move |(idx, line)| {
            let l_st = start;
            start += line.len() + 1;
            (idx, l_st, start - 1, line)
        })
    };

    info!("Extracted Lines");

    let sections = {
        lines.flat_map(move |(lidx, l_st, l_en, line)| {
            let mut sec_offset = l_st;
            line.split(',').map(move |section| {
                let s_st = sec_offset;
                sec_offset += section.len() + 1;

                (lidx, (l_st, l_en), (s_st, sec_offset - 1), section)
            })
        })
    };

    info!("Extracted Sections");

    let mut out = Vec::new();

    for (lidx, (l_st, l_en), (s_st, s_en), section) in sections {
        let tokens = {
            let mut token_offset: usize = s_st;
            section.split(' ').map(move |token: &str| {
                let t_st = token_offset;
                token_offset += token.len() + 1;

                ((t_st, token_offset - 1), token)
            })
        };

        let section_span = Rc::new(SectionSpan {
            l_idx: lidx,
            l_bs: (l_st, l_en),
            s_bs: (s_st, s_en),
        });

        let mut token_list = Vec::new();

        for ((t_st, t_en), token) in tokens {
            let mut offset = 0;

            while token.chars().nth(offset) == Some(' ') {
                offset += 1;
            }

            if !(token.len() - offset == 0) {
                token_list.push(Rc::new(TokenSpan {
                    section: section_span.clone(),
                    t_bs: (t_st + offset, t_en),
                }));
            }
        }

        out.push(Section {
            span: section_span,
            token: token_list,
        });
    }

    info!("Extracted tokens");

    info!("Finished pre-lex");
    Ok(PreLexed {
        instr,
        sections: out,
    })
}
// }}}

#[derive(Debug)]
struct Parsed<'a> {
    instr: &'a str,
    lines: Vec<Line<Rc<TokenSpan>>>,
}

fn get_token<'a>(token: &TokenSpan, instr: &'a str) -> &'a str {
    &instr[token.t_bs.0..token.t_bs.1]
}

fn parse<'a>(pre_lex: PreLexed<'a>) -> Result<Parsed<'a>, AssembleError<'a>> {
    info!("Begging Parse step");

    let instr = pre_lex.instr;

    let mut lines = Vec::new();

    for Section { span, token } in pre_lex.sections.into_iter() {
        let token_count = token.len();
        if token_count == 0 {
            warn!(
                "Skipping instruction on line {} as there are no tokens",
                span.l_idx
            );
            continue;
        } else if token_count > 3 {
            error!(
                "Found instruction with wrong number of tokens {} on line {}: {}",
                token_count,
                span.l_idx,
                &instr[span.s_bs.0..span.s_bs.1]
            );
            return Err(AssembleError {
                instr,
                span: span.into(),
                message: AssembleErrorType::MalformedInstruction(1, 3, token_count),
            });
        }

        let instr_tex = get_token(&token[0], instr);

        let line = match instr_tex {
            "AND" => {
                if token_count == 2 {
                    Line::Instruction {
                        span,
                        addr: None,
                        instr: Instruction::AND(token[1].clone()),
                    }
                } else {
                    let indf = get_token(&token[2], instr);
                    if indf == "I" {
                        Line::Instruction {
                            span,
                            addr: None,
                            instr: Instruction::ANDI(token[1].clone()),
                        }
                    } else {
                        return Err(AssembleError {
                            instr,
                            span: token[2].clone().into(),
                            message: AssembleErrorType::ExpectedIndirectionArgument(indf),
                        });
                    }
                }
            }

            "ADD" => {
                if token_count == 2 {
                    Line::Instruction {
                        span,
                        addr: None,
                        instr: Instruction::ADD(token[1].clone()),
                    }
                } else {
                    let indf = get_token(&token[2], instr);
                    if indf == "I" {
                        Line::Instruction {
                            span,
                            addr: None,
                            instr: Instruction::ADDI(token[1].clone()),
                        }
                    } else {
                        return Err(AssembleError {
                            instr,
                            span: token[2].clone().into(),
                            message: AssembleErrorType::ExpectedIndirectionArgument(indf),
                        });
                    }
                }
            }

            "LDA" => {
                if token_count == 2 {
                    Line::Instruction {
                        span,
                        addr: None,
                        instr: Instruction::LDA(token[1].clone()),
                    }
                } else {
                    let indf = get_token(&token[2], instr);
                    if indf == "I" {
                        Line::Instruction {
                            span,
                            addr: None,
                            instr: Instruction::LDAI(token[1].clone()),
                        }
                    } else {
                        return Err(AssembleError {
                            instr,
                            span: token[2].clone().into(),
                            message: AssembleErrorType::ExpectedIndirectionArgument(indf),
                        });
                    }
                }
            }

            "STA" => {
                if token_count == 2 {
                    Line::Instruction {
                        span,
                        addr: None,
                        instr: Instruction::STA(token[1].clone()),
                    }
                } else {
                    let indf = get_token(&token[2], instr);
                    if indf == "I" {
                        Line::Instruction {
                            span,
                            addr: None,
                            instr: Instruction::STAI(token[1].clone()),
                        }
                    } else {
                        return Err(AssembleError {
                            instr,
                            span: token[2].clone().into(),
                            message: AssembleErrorType::ExpectedIndirectionArgument(indf),
                        });
                    }
                }
            }

            "BUN" => {
                if token_count == 2 {
                    Line::Instruction {
                        span,
                        addr: None,
                        instr: Instruction::BUN(token[1].clone()),
                    }
                } else {
                    let indf = get_token(&token[2], instr);
                    if indf == "I" {
                        Line::Instruction {
                            span,
                            addr: None,
                            instr: Instruction::BUNI(token[1].clone()),
                        }
                    } else {
                        return Err(AssembleError {
                            instr,
                            span: token[2].clone().into(),
                            message: AssembleErrorType::ExpectedIndirectionArgument(indf),
                        });
                    }
                }
            }

            "BSA" => {
                if token_count == 2 {
                    Line::Instruction {
                        span,
                        addr: None,
                        instr: Instruction::BSA(token[1].clone()),
                    }
                } else {
                    let indf = get_token(&token[2], instr);
                    if indf == "I" {
                        Line::Instruction {
                            span,
                            addr: None,
                            instr: Instruction::BSAI(token[1].clone()),
                        }
                    } else {
                        return Err(AssembleError {
                            instr,
                            span: token[2].clone().into(),
                            message: AssembleErrorType::ExpectedIndirectionArgument(indf),
                        });
                    }
                }
            }

            "ISZ" => {
                if token_count == 2 {
                    Line::Instruction {
                        span,
                        addr: None,
                        instr: Instruction::ISZ(token[1].clone()),
                    }
                } else {
                    let indf = get_token(&token[2], instr);
                    if indf == "I" {
                        Line::Instruction {
                            span,
                            addr: None,
                            instr: Instruction::ISZI(token[1].clone()),
                        }
                    } else {
                        return Err(AssembleError {
                            instr,
                            span: token[2].clone().into(),
                            message: AssembleErrorType::ExpectedIndirectionArgument(indf),
                        });
                    }
                }
            }

            "CLA" => {
                if token_count == 1 {
                    Line::Instruction {
                        span,
                        addr: None,
                        instr: Instruction::CLA,
                    }
                } else {
                    return Err(AssembleError {
                        instr,
                        span: span.clone().into(),
                        message: AssembleErrorType::NoArgumentsExpected("CLA", token_count - 1),
                    });
                }
            }

            "CLE" => {
                if token_count == 1 {
                    Line::Instruction {
                        span,
                        addr: None,
                        instr: Instruction::CLE,
                    }
                } else {
                    return Err(AssembleError {
                        instr,
                        span: span.clone().into(),
                        message: AssembleErrorType::NoArgumentsExpected("CLE", token_count - 1),
                    });
                }
            }

            "CMA" => {
                if token_count == 1 {
                    Line::Instruction {
                        span,
                        addr: None,
                        instr: Instruction::CMA,
                    }
                } else {
                    return Err(AssembleError {
                        instr,
                        span: span.clone().into(),
                        message: AssembleErrorType::NoArgumentsExpected("CMA", token_count - 1),
                    });
                }
            }

            "CME" => {
                if token_count == 1 {
                    Line::Instruction {
                        span,
                        addr: None,
                        instr: Instruction::CME,
                    }
                } else {
                    return Err(AssembleError {
                        instr,
                        span: span.clone().into(),
                        message: AssembleErrorType::NoArgumentsExpected("CME", token_count - 1),
                    });
                }
            }

            "CIR" => {
                if token_count == 1 {
                    Line::Instruction {
                        span,
                        addr: None,
                        instr: Instruction::CIR,
                    }
                } else {
                    return Err(AssembleError {
                        instr,
                        span: span.clone().into(),
                        message: AssembleErrorType::NoArgumentsExpected("CIR", token_count - 1),
                    });
                }
            }

            "CIL" => {
                if token_count == 1 {
                    Line::Instruction {
                        span,
                        addr: None,
                        instr: Instruction::CIL,
                    }
                } else {
                    return Err(AssembleError {
                        instr,
                        span: span.clone().into(),
                        message: AssembleErrorType::NoArgumentsExpected("CIL", token_count - 1),
                    });
                }
            }

            "INC" => {
                if token_count == 1 {
                    Line::Instruction {
                        span,
                        addr: None,
                        instr: Instruction::INC,
                    }
                } else {
                    return Err(AssembleError {
                        instr,
                        span: span.clone().into(),
                        message: AssembleErrorType::NoArgumentsExpected("INC", token_count - 1),
                    });
                }
            }

            "SPA" => {
                if token_count == 1 {
                    Line::Instruction {
                        span,
                        addr: None,
                        instr: Instruction::SPA,
                    }
                } else {
                    return Err(AssembleError {
                        instr,
                        span: span.clone().into(),
                        message: AssembleErrorType::NoArgumentsExpected("SPA", token_count - 1),
                    });
                }
            }

            "SNA" => {
                if token_count == 1 {
                    Line::Instruction {
                        span,
                        addr: None,
                        instr: Instruction::SNA,
                    }
                } else {
                    return Err(AssembleError {
                        instr,
                        span: span.clone().into(),
                        message: AssembleErrorType::NoArgumentsExpected("SNA", token_count - 1),
                    });
                }
            }

            "SZA" => {
                if token_count == 1 {
                    Line::Instruction {
                        span,
                        addr: None,
                        instr: Instruction::SZA,
                    }
                } else {
                    return Err(AssembleError {
                        instr,
                        span: span.clone().into(),
                        message: AssembleErrorType::NoArgumentsExpected("SZA", token_count - 1),
                    });
                }
            }

            "SZE" => {
                if token_count == 1 {
                    Line::Instruction {
                        span,
                        addr: None,
                        instr: Instruction::SZE,
                    }
                } else {
                    return Err(AssembleError {
                        instr,
                        span: span.clone().into(),
                        message: AssembleErrorType::NoArgumentsExpected("SZE", token_count - 1),
                    });
                }
            }

            "HLT" => {
                if token_count == 1 {
                    Line::Instruction {
                        span,
                        addr: None,
                        instr: Instruction::HLT,
                    }
                } else {
                    return Err(AssembleError {
                        instr,
                        span: span.clone().into(),
                        message: AssembleErrorType::NoArgumentsExpected("HLT", token_count - 1),
                    });
                }
            }

            "INP" => {
                if token_count == 1 {
                    Line::Instruction {
                        span,
                        addr: None,
                        instr: Instruction::INP,
                    }
                } else {
                    return Err(AssembleError {
                        instr,
                        span: span.clone().into(),
                        message: AssembleErrorType::NoArgumentsExpected("INP", token_count - 1),
                    });
                }
            }

            "OUT" => {
                if token_count == 1 {
                    Line::Instruction {
                        span,
                        addr: None,
                        instr: Instruction::OUT,
                    }
                } else {
                    return Err(AssembleError {
                        instr,
                        span: span.clone().into(),
                        message: AssembleErrorType::NoArgumentsExpected("OUT", token_count - 1),
                    });
                }
            }

            "SKI" => {
                if token_count == 1 {
                    Line::Instruction {
                        span,
                        addr: None,
                        instr: Instruction::SKI,
                    }
                } else {
                    return Err(AssembleError {
                        instr,
                        span: span.clone().into(),
                        message: AssembleErrorType::NoArgumentsExpected("SKI", token_count - 1),
                    });
                }
            }

            "SKO" => {
                if token_count == 1 {
                    Line::Instruction {
                        span,
                        addr: None,
                        instr: Instruction::SKO,
                    }
                } else {
                    return Err(AssembleError {
                        instr,
                        span: span.clone().into(),
                        message: AssembleErrorType::NoArgumentsExpected("SKO", token_count - 1),
                    });
                }
            }

            "ION" => {
                if token_count == 1 {
                    Line::Instruction {
                        span,
                        addr: None,
                        instr: Instruction::ION,
                    }
                } else {
                    return Err(AssembleError {
                        instr,
                        span: span.clone().into(),
                        message: AssembleErrorType::NoArgumentsExpected("ION", token_count - 1),
                    });
                }
            }

            "IOF" => {
                if token_count == 1 {
                    Line::Instruction {
                        span,
                        addr: None,
                        instr: Instruction::IOF,
                    }
                } else {
                    return Err(AssembleError {
                        instr,
                        span: span.clone().into(),
                        message: AssembleErrorType::NoArgumentsExpected("IOF", token_count - 1),
                    });
                }
            }

            "HEX" => {
                if token_count == 2 {
                    if let Ok(value) = u16::from_str_radix(get_token(&token[1], instr), 16) {
                        Line::Directive {
                            span,
                            addr: None,
                            dir: Directive::HEX(value),
                        }
                    } else {
                        return Err(AssembleError {
                            instr,
                            span: token[1].clone().into(),
                            message: AssembleErrorType::LiteralParseError(
                                "Expected hex integer literal",
                            ),
                        });
                    }
                } else {
                    return Err(AssembleError {
                        instr,
                        span: span.into(),
                        message: AssembleErrorType::MalformedArguments("HEX", 1, 1, token_count),
                    });
                }
            }

            "DEC" => {
                if token_count == 2 {
                    let (mult, rspan) = if instr.chars().nth(token[1].t_bs.0) == Some('-') {
                        (-1, (token[1].t_bs.0 + 1, token[1].t_bs.1))
                    } else {
                        (1, token[1].t_bs)
                    };

                    if let Ok(value) = i16::from_str_radix(&instr[rspan.0..rspan.1], 10) {
                        Line::Directive {
                            span,
                            addr: None,
                            dir: Directive::DEC(mult * value),
                        }
                    } else {
                        return Err(AssembleError {
                            instr,
                            span: token[1].clone().into(),
                            message: AssembleErrorType::LiteralParseError(
                                "Expected decimal integer literal",
                            ),
                        });
                    }
                } else {
                    return Err(AssembleError {
                        instr,
                        span: span.into(),
                        message: AssembleErrorType::MalformedArguments("DEC", 1, 1, token_count),
                    });
                }
            }

            "ORG" => {
                if token_count == 2 {
                    if let Ok(org) = u16::from_str_radix(get_token(&token[1], instr), 16) {
                        Line::Directive {
                            span,
                            addr: None,
                            dir: Directive::ORG(org),
                        }
                    } else {
                        return Err(AssembleError {
                            instr,
                            span: token[1].clone().into(),
                            message: AssembleErrorType::LiteralParseError(
                                "Expected address in hex form",
                            ),
                        });
                    }
                } else {
                    return Err(AssembleError {
                        instr,
                        span: span.into(),
                        message: AssembleErrorType::MalformedArguments("ORG", 1, 1, token_count),
                    });
                }
            }

            _ => {
                if token_count == 1 {
                    info!(
                        "Treating as label directive: {}",
                        get_token(&token[0], instr)
                    );

                    Line::Directive {
                        span,
                        addr: None,
                        dir: Directive::Label(token[0].clone()),
                    }
                } else {
                    error!(
                        "Instruction {} on line {} did not exist.",
                        get_token(&token[0], instr),
                        token[0].section.l_idx,
                    );

                    return Err(AssembleError {
                        instr,
                        span: token[0].clone().into(),
                        message: AssembleErrorType::InvalidInstruction(get_token(&token[0], instr)),
                    });
                }
            }
        };

        lines.push(line);
    }

    info!("End Parse step");
    Ok(Parsed { instr, lines })
}

#[derive(Debug)]
struct LaidOut<'a> {
    instr: &'a str,
    lines: Vec<Line<Rc<TokenSpan>>>,
    labels: HashMap<&'a str, u16>,
    max: u16,
}

fn origins<'a>(Parsed { instr, mut lines }: Parsed<'a>) -> Result<LaidOut<'a>, AssembleError<'a>> {
    let mut org: u16 = 0;
    let mut next_label: Option<&'a str> = None;
    let mut labels = HashMap::new();

    for line in lines.iter_mut() {
        let mut skip = false;

        match line {
            Line::Directive { span, addr, dir } => match dir {
                Directive::ORG(new_org) => {
                    skip = true;

                    if *new_org < org {
                        return Err(AssembleError {
                            instr,
                            span: span.clone().into(),
                            message: AssembleErrorType::InvalidOrgDirective(
                                org as usize,
                                *new_org as usize,
                            ),
                        });
                    }

                    org = *new_org;
                }
                Directive::Label(lab) => {
                    skip = true;

                    let lab = get_token(&lab, instr);
                    if let Some(next_label) = next_label {
                        return Err(AssembleError {
                            instr,
                            span: span.clone().into(),
                            message: AssembleErrorType::DoubleLabelError(next_label, lab),
                        });
                    }

                    next_label.replace(lab);
                }
                _ => {
                    addr.replace(org);
                }
            },
            Line::Instruction {
                span: _,
                addr,
                instr: _,
            } => {
                addr.replace(org);
            }
        }

        if let Some(lab) = next_label.take() {
            labels.insert(lab, org);
        }

        if !skip {
            org += 1;
        }
    }

    Ok(LaidOut {
        instr,
        lines,
        labels,
        max: org,
    })
}

#[derive(Debug)]
struct Lowered<'a> {
    instr: &'a str,
    lines: Vec<Line<u16>>,
    max: u16,
}

fn lower_references<'a>(
    LaidOut {
        instr,
        lines,
        labels,
        max,
    }: LaidOut<'a>,
) -> Result<Lowered<'a>, AssembleError<'a>> {
    info!("Starting lower references");

    let mut out_lines = Vec::with_capacity(lines.len());

    for line in lines.into_iter() {
        match line {
            Line::Instruction {
                span,
                addr,
                instr: i,
            } => {
                let labels = &labels;
                out_lines.push(Line::Instruction {
                    span: span.clone(),
                    addr,
                    instr: i.try_map(move |r| {
                        let lab = get_token(&r, instr);
                        if let Some(lab) = labels.get(lab) {
                            Ok(*lab)
                        } else {
                            Err(AssembleError {
                                instr,
                                span: span.clone().into(),
                                message: AssembleErrorType::MissingReference(lab),
                            })
                        }
                    })?,
                });
            }

            Line::Directive { span, addr, dir } => match dir {
                Directive::HEX(a) => out_lines.push(Line::Directive {
                    span,
                    addr,
                    dir: Directive::HEX(a),
                }),
                Directive::DEC(a) => out_lines.push(Line::Directive {
                    span,
                    addr,
                    dir: Directive::DEC(a),
                }),
                _ => (),
            },
        }
    }

    info!("Finished lower reference");
    Ok(Lowered {
        instr,
        lines: out_lines,
        max,
    })
}

#[derive(Debug)]
// {{{ Docs
/**
 * Represents a final assembled mano program which can be read by the rano interpreter.
 *
 * May include debug information if assembled with [`assemble_debug`].
 *
 * ```
 * # use rano::ass::*;
 * fn main() -> Result<(), AssembleError<'static>> {
 *     let mano = "ORG 100\nVAR1, HEX 0010\nORG 130\nLDA VAR1\nADD VAR1";
 *
 *     match assemble_debug(mano) {
 *         Ok(fin) => /* Write to file */,
 *         Err(e) => {
 *             // we print this here to get the display version of the error
 *             // instead of the debug version
 *             println!("Error encountered:\n{}", e);
 *             # panic!("Example should not fail to assemble");
 *         }
 *     }
 *
 * }
 * ```
 *
 * # File layout
 *
 * ```none
 *     LAYOUT
 *  *-----------*
 *  |  Header   |
 *  *-----------*
 *  |   text    |
 *  |           |
 *  |           |
 *  *-----------*
 *  |   spans   |
 *  |           |
 *  *-----------*
 *  |    src    |
 *  |           |
 *  |           |
 *  |           |
 *  *-----------*
 * ```
 *
 * If compiled without debug symbols then the spans and src sections are 0 sized
 *
 * ### Header layout
 *
 * The header layout is slightly different depending on if the src was assembled with or
 * without debug symbols. In the case that debug is 0 span_offset and src_offset are removed.
 * Offsets are relative to the beginning of the file.
 *
 * ```none
 * 0        4      5      6             8            12           16
 * *--------*------*------*--------*----*-------------*------------*
 * | "RANO" | VMaj | Vmin | debug? |    | span_offset | src_offset |
 * |   4B   |  1B  |  1B  |   1B   | 1B |     4B      |     4B     |
 * *--------*------*------*--------*----*-------------*------------*
 * ```
 *
 */ // }}}
pub struct Assembled {
    data: Vec<u8>,
}

union Instr {
    edit: u16,
    fin: [u8; 2],
}

impl Default for Instr {
    fn default() -> Self {
        Instr { edit: 0u16 }
    }
}

impl Instr {
    fn write_to_at(&self, to: &mut Vec<u8>, at: usize) {
        // SAFETY: any valid bitpattern for a u16 can be interpreted as an array of 2 u8s with
        // valid bitpatterns
        for (offset, byte) in unsafe { self.fin }.iter().enumerate() {
            to[at * 2 + offset] = *byte;
        }
    }
}

fn assemble_lines<'a>(
    Lowered {
        instr: _,
        lines,
        max,
    }: Lowered,
) -> Assembled {
    info!("Starting assembling individual instruction");
    let cap = max as usize * 2 + HEADER_SIZE;
    let mut data = Vec::with_capacity(max as usize);
    data.resize(max as usize * 2 + 4, 0);

    // Put the file header in
    for (idx, i) in b"rano".iter().enumerate() {
        data[idx] = *i;
    }

    // Indicate that there are no debug symbols
    data[4] = CURRENT_VERSION.0;
    data[5] = CURRENT_VERSION.1;
    data[6] = 0u8;

    info!("Finished writing header: {:?}", &data[0..8]);

    for line in lines.into_iter() {
        Instr {
            edit: line.as_u16(),
        }
        .write_to_at(&mut data, line.addr().unwrap() as usize + 8);
    }

    info!("Finished assembling individual instruction");
    Assembled { data }
}

fn assemble_debug_lines<'a>(Lowered { instr, lines, max }: Lowered) -> Assembled {
    info!("Starting assembling with debug information");

    union Offsets {
        value: u32,
        bytes: [u8; 4],
    }

    union SpanInfo {
        value: (u32, u32, u32),
        bytes: [u8; 12],
    }

    fn write_to_at(bytes: &[u8], to: &mut Vec<u8>, at: usize) {
        for (idx, i) in bytes.iter().enumerate() {
            to[idx + at] = *i;
        }
    }

    let cap = max as usize * 2 + lines.len() * 12 + instr.len() + HEADER_SIZE;
    let mut data = Vec::with_capacity(cap);
    data.resize(cap, 0);

    for (idx, i) in b"rano".iter().enumerate() {
        data[idx] = *i;
    }

    data[4] = CURRENT_VERSION.0;
    data[5] = CURRENT_VERSION.1;
    data[6] = 1u8;

    let mut cursor = 8;

    for line in lines.iter() {
        Instr {
            edit: line.as_u16(),
        }
        .write_to_at(&mut data, line.addr().unwrap() as usize + cursor);
    }

    info!("Finished writing program text to file");
    cursor += max as usize * 2;

    write_to_at(
        unsafe {
            &Offsets {
                value: cursor as u32,
            }
            .bytes
        },
        &mut data,
        8,
    );

    for line in lines.iter() {
        let span = match line {
            Line::Instruction { span, .. } => span,
            Line::Directive { span, .. } => span,
        };
        write_to_at(
            unsafe {
                &SpanInfo {
                    value: (span.l_idx as u32, span.s_bs.0 as u32, span.s_bs.1 as u32),
                }
                .bytes
            },
            &mut data,
            cursor,
        );

        cursor += 12;
    }

    info!("Finished writing span information");

    write_to_at(
        unsafe {
            &Offsets {
                value: cursor as u32,
            }
            .bytes
        },
        &mut data,
        12,
    );

    for b in instr.bytes() {
        data[cursor] = b;
        cursor += 1;
    }

    info!("Finished assembling with debug information");
    Assembled { data }
}

impl Assembled {
    /// Get the internal buffer for the assembled program
    pub fn buffer(&self) -> &[u8] {
        &self.data[..]
    }
}

/// Assemble a stripped version of a mano binary file to be run by the rano interpreter
///
/// See [`Assembled`] for more information on the file layout
pub fn assemble_str<'a>(instr: &'a str) -> Result<Assembled, AssembleError<'a>> {
    pre_lex(instr)
        .and_then(parse)
        .and_then(origins)
        .and_then(lower_references)
        .map(assemble_lines)
}

/// Assemble a mano binary file with debug information to be run by the rano interpreter
///
/// See [`Assembled`] for more information on the file layout
pub fn assemble_debug<'a>(instr: &'a str) -> Result<Assembled, AssembleError<'a>> {
    pre_lex(instr)
        .and_then(parse)
        .and_then(origins)
        .and_then(lower_references)
        .map(assemble_debug_lines)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pre_lex() {
        let ins = "VAR1, HEX 120\nVAR2, DEC 000\nORG 0010\nADD VAR1\nADD VAR2 I";
        match assemble_str(ins) {
            Ok(v) => println!("{:?}", v),
            Err(e) => {
                eprintln!("{}", e);
                panic!("Failed to compile");
            }
        }
    }
}
