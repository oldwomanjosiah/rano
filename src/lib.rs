use std::{collections::HashMap, fmt::Debug, rc::Rc};

extern crate console;
extern crate log;
extern crate nom;
extern crate thiserror;

use console::Style;
use log::error;
use log::info;
use thiserror::Error;

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
    ADD(Reference),
    ADDI(Reference),
}

impl<T: Debug> Instruction<T> {
    pub fn map<U: Debug>(self, f: impl Fn(T) -> U) -> Instruction<U> {
        match self {
            Instruction::ADD(t) => Instruction::ADD(f(t)),
            Instruction::ADDI(t) => Instruction::ADDI(f(t)),
        }
    }

    pub fn try_map<U: Debug, E: std::error::Error>(
        self,
        f: impl Fn(T) -> Result<U, E>,
    ) -> Result<Instruction<U>, E> {
        match self.map(f) {
            Instruction::ADD(Ok(t)) => Ok(Instruction::ADD(t)),
            Instruction::ADD(Err(e)) => Err(e),

            Instruction::ADDI(Ok(t)) => Ok(Instruction::ADDI(t)),
            Instruction::ADDI(Err(e)) => Err(e),
        }
    }
}

#[derive(Debug)]
enum Directive<Reference: Debug> {
    ORG(u32),
    Label(Reference),
    HEX(u32),
    DEC(i32),
}

#[derive(Debug)]
enum Line<Reference: Debug> {
    Instruction {
        span: Rc<SectionSpan>,
        addr: Option<u32>,
        instr: Instruction<Reference>,
    },
    Directive {
        span: Rc<SectionSpan>,
        addr: Option<u32>,
        dir: Directive<Reference>,
    },
}

#[derive(Debug)]
struct Context {
    memloc: u32,
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
        writeln!(f, "On Line: {}", self.span.line())?;
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
        if token_count < 1 || token_count > 3 {
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

            "HEX" => {
                if token_count == 2 {
                    if let Ok(value) = u32::from_str_radix(get_token(&token[1], instr), 16) {
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

                    if let Ok(value) = i32::from_str_radix(&instr[rspan.0..rspan.1], 10) {
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
                    if let Ok(org) = u32::from_str_radix(get_token(&token[1], instr), 16) {
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
    labels: HashMap<&'a str, u32>,
    max: u32,
}

fn origins<'a>(Parsed { instr, mut lines }: Parsed<'a>) -> Result<LaidOut<'a>, AssembleError<'a>> {
    let mut org: u32 = 0;
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
struct Lowered {
    lines: Vec<Line<u32>>,
    max: u32,
}

fn lower_references<'a>(
    LaidOut {
        instr,
        lines,
        labels,
        max,
    }: LaidOut<'a>,
) -> Result<Lowered, AssembleError<'a>> {
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
        lines: out_lines,
        max,
    })
}

#[derive(Debug)]
struct Assembled {
    data: Vec<u8>,
}

fn assemble_lines<'a>(lines: Lowered) -> Assembled {
    eprintln!("{:#?}", lines);
    todo!();
}

pub fn assemble_str<'a>(instr: &'a str) -> Result<Vec<u8>, AssembleError<'a>> {
    pre_lex(instr)
        .and_then(parse)
        .and_then(origins)
        .and_then(lower_references)
        .map(assemble_lines)
        .map(|a| a.data)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pre_lex() {
        let ins = "VAR1, HEX 120\nVAR2, DEC 120\nORG 0010\nADD VAR1\nADD VAR2 I";
        match assemble_str(ins) {
            Ok(v) => println!("{:?}", v),
            Err(e) => {
                eprintln!("{}", e);
                panic!("Failed to compile");
            }
        }
    }
}
