use std::collections::HashMap;

use super::*;
use log::{debug, info};

// Represents the divisor for chars used to initalize tokens vec
const TOK_HEUR: usize = 4;
const LIN_HEUR: usize = 12;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Terminal {
    Comma,
    Newline,
    CommentStart,

    // Memory ops
    And,
    Add,
    Lda,
    Sta,
    Bun,
    Bsa,
    Isz,

    Indirection,

    // Register ops
    Cla,
    Cle,
    Cma,
    Cme,
    Cir,
    Cil,
    Inc,
    Spa,
    Sna,
    Sze,
    Hlt,

    // IO ops
    Inp,
    Out,
    Ski,
    Sko,
    Ion,
    Iof,

    // Directives
    Org,
    Hex,
    Dec,
}

#[derive(Debug, PartialEq, Eq)]
pub enum TokenVal {
    Terminal(Terminal),
    NonTerminal,
    Ignored,
}

#[derive(Debug)]
pub struct Token {
    pub span: Span,
    pub tval: TokenVal,
}

#[derive(Debug)]
pub struct Tokens<'a> {
    pub ctx: ParseContext<'a>,
    pub tokens: Box<[Token]>,
}

pub fn lex(instr: &str) -> Result<Tokens> {
    info!("Beginning lex step with token heur of {}", TOK_HEUR);
    info!("Beginning lex step with line heur of {}", LIN_HEUR);

    let len = instr.len();

    let mut offset_current = 0;
    let mut left = &instr[..];

    let mut tokens = Vec::with_capacity(len / TOK_HEUR);

    let mut line = 1;

    let mut line_span: Vec<Span> = Vec::with_capacity(8);
    // TODO change to be line count heur (or import heur from parse)
    let mut lines: HashMap<u32, Span> = HashMap::with_capacity(len / LIN_HEUR);

    while offset_current < len {
        let lc = len - offset_current;

        if &left[0..1] == " " || &left[0..1] == "\t" {
            offset_current += 1;
            left = &left[1..];
        } else if &left[0..1] == "\n" {
            let span = Span::new_unchecked(offset_current, offset_current + 1).line(line);
            tokens.push(Token {
                span,
                tval: TokenVal::Terminal(Terminal::Newline),
            });

            let mut spans = line_span.drain(..);
            if let Some(first) = spans.next() {
                lines.insert(line as u32, spans.fold(first, |a, c| a.join(c)));
            } else {
                lines.insert(
                    line as u32,
                    Span::new_unchecked(offset_current, offset_current).line(line),
                );
            }

            offset_current += 1;
            left = &left[1..];

            line += 1;
        } else if &left[0..2] == "\r\n" {
            let span = Span::new_unchecked(offset_current, offset_current + 2).line(line);
            tokens.push(Token {
                span,
                tval: TokenVal::Terminal(Terminal::Newline),
            });

            let mut spans = line_span.drain(..);
            if let Some(first) = spans.next() {
                lines.insert(line as u32, spans.fold(first, |a, c| a.join(c)));
            } else {
                lines.insert(
                    line as u32,
                    Span::new_unchecked(offset_current, offset_current).line(line),
                );
            }

            offset_current += 2;
            left = &left[2..];

            line += 1;
        } else if &left[0..1] == "," {
            let span = Span::new_unchecked(offset_current, offset_current + 1).line(line);
            line_span.push(span);

            tokens.push(Token {
                span,
                tval: TokenVal::Terminal(Terminal::Comma),
            });

            offset_current += 1;
            left = &left[1..];
        } else if &left[0..1] == ";" || &left[0..1] == "#" {
            let span = Span::new_unchecked(offset_current, offset_current + 1).line(line);
            line_span.push(span);

            tokens.push(Token {
                span,
                tval: TokenVal::Terminal(Terminal::CommentStart),
            });

            offset_current += 1;
            left = &left[1..];

            // Eat whitespace off beginning of comment
            while &left[0..1] == " " || &left[0..1] == "\t" {
                offset_current += 1;
                left = &left[1..];
            }

            // Skip lexing the rest of the comment
            let mut end = 1;
            loop {
                if end + offset_current >= len {
                    break;
                }
                match &left[end..=end] {
                    "\n" | "\r" => break,
                    _ => end += 1,
                }
            }

            // Keep token for rest of comment so it can be recreated if needed
            let span = Span::new_unchecked(offset_current, offset_current + end).line(line);
            line_span.push(span);

            tokens.push(Token {
                span,
                tval: TokenVal::Ignored,
            });

            debug!("Ignored comment text with len {}: {}", end, &left[..end]);
            debug!("Broke on char {:?}", &left[end..=end]);

            offset_current += end; // TODO Might have to change this to end - 1
            left = &left[end..];
        } else if lc >= 3 && left[..3].eq_ignore_ascii_case("AND") {
            let span = Span::new_unchecked(offset_current, offset_current + 3).line(line);
            line_span.push(span);

            tokens.push(Token {
                span,
                tval: TokenVal::Terminal(Terminal::And),
            });

            offset_current += 3;
            left = &left[3..];
        } else if lc >= 3 && left[..3].eq_ignore_ascii_case("ADD") {
            let span = Span::new_unchecked(offset_current, offset_current + 3).line(line);
            line_span.push(span);

            tokens.push(Token {
                span,
                tval: TokenVal::Terminal(Terminal::Add),
            });

            offset_current += 3;
            left = &left[3..];
        } else if lc >= 3 && left[..3].eq_ignore_ascii_case("LDA") {
            let span = Span::new_unchecked(offset_current, offset_current + 3).line(line);
            line_span.push(span);

            tokens.push(Token {
                span,
                tval: TokenVal::Terminal(Terminal::Lda),
            });

            offset_current += 3;
            left = &left[3..];
        } else if lc >= 3 && left[..3].eq_ignore_ascii_case("STA") {
            let span = Span::new_unchecked(offset_current, offset_current + 3).line(line);
            line_span.push(span);

            tokens.push(Token {
                span,
                tval: TokenVal::Terminal(Terminal::Sta),
            });

            offset_current += 3;
            left = &left[3..];
        } else if lc >= 3 && left[..3].eq_ignore_ascii_case("BUN") {
            let span = Span::new_unchecked(offset_current, offset_current + 3).line(line);
            line_span.push(span);

            tokens.push(Token {
                span,
                tval: TokenVal::Terminal(Terminal::Bun),
            });

            offset_current += 3;
            left = &left[3..];
        } else if lc >= 3 && left[..3].eq_ignore_ascii_case("BSA") {
            let span = Span::new_unchecked(offset_current, offset_current + 3).line(line);
            line_span.push(span);

            tokens.push(Token {
                span,
                tval: TokenVal::Terminal(Terminal::Bsa),
            });

            offset_current += 3;
            left = &left[3..];
        } else if lc >= 3 && left[..3].eq_ignore_ascii_case("ISZ") {
            let span = Span::new_unchecked(offset_current, offset_current + 3).line(line);
            line_span.push(span);

            tokens.push(Token {
                span,
                tval: TokenVal::Terminal(Terminal::Isz),
            });

            offset_current += 3;
            left = &left[3..];
        } else if lc >= 3 && left[..3].eq_ignore_ascii_case("CLA") {
            let span = Span::new_unchecked(offset_current, offset_current + 3).line(line);
            line_span.push(span);

            tokens.push(Token {
                span,
                tval: TokenVal::Terminal(Terminal::Cla),
            });

            offset_current += 3;
            left = &left[3..];
        } else if lc >= 3 && left[..3].eq_ignore_ascii_case("CLE") {
            let span = Span::new_unchecked(offset_current, offset_current + 3).line(line);
            line_span.push(span);

            tokens.push(Token {
                span,
                tval: TokenVal::Terminal(Terminal::Cle),
            });

            offset_current += 3;
            left = &left[3..];
        } else if lc >= 3 && left[..3].eq_ignore_ascii_case("CMA") {
            let span = Span::new_unchecked(offset_current, offset_current + 3).line(line);
            line_span.push(span);

            tokens.push(Token {
                span,
                tval: TokenVal::Terminal(Terminal::Cma),
            });

            offset_current += 3;
            left = &left[3..];
        } else if lc >= 3 && left[..3].eq_ignore_ascii_case("CME") {
            let span = Span::new_unchecked(offset_current, offset_current + 3).line(line);
            line_span.push(span);

            tokens.push(Token {
                span,
                tval: TokenVal::Terminal(Terminal::Cme),
            });

            offset_current += 3;
            left = &left[3..];
        } else if lc >= 3 && left[..3].eq_ignore_ascii_case("CIR") {
            let span = Span::new_unchecked(offset_current, offset_current + 3).line(line);
            line_span.push(span);

            tokens.push(Token {
                span,
                tval: TokenVal::Terminal(Terminal::Cir),
            });

            offset_current += 3;
            left = &left[3..];
        } else if lc >= 3 && left[..3].eq_ignore_ascii_case("CIL") {
            let span = Span::new_unchecked(offset_current, offset_current + 3).line(line);
            line_span.push(span);

            tokens.push(Token {
                span,
                tval: TokenVal::Terminal(Terminal::Cil),
            });

            offset_current += 3;
            left = &left[3..];
        } else if lc >= 3 && left[..3].eq_ignore_ascii_case("INC") {
            let span = Span::new_unchecked(offset_current, offset_current + 3).line(line);
            line_span.push(span);

            tokens.push(Token {
                span,
                tval: TokenVal::Terminal(Terminal::Inc),
            });

            offset_current += 3;
            left = &left[3..];
        } else if lc >= 3 && left[..3].eq_ignore_ascii_case("SPA") {
            let span = Span::new_unchecked(offset_current, offset_current + 3).line(line);
            line_span.push(span);

            tokens.push(Token {
                span,
                tval: TokenVal::Terminal(Terminal::Spa),
            });

            offset_current += 3;
            left = &left[3..];
        } else if lc >= 3 && left[..3].eq_ignore_ascii_case("SNA") {
            let span = Span::new_unchecked(offset_current, offset_current + 3).line(line);
            line_span.push(span);

            tokens.push(Token {
                span,
                tval: TokenVal::Terminal(Terminal::Sna),
            });

            offset_current += 3;
            left = &left[3..];
        } else if lc >= 3 && left[..3].eq_ignore_ascii_case("SZE") {
            let span = Span::new_unchecked(offset_current, offset_current + 3).line(line);
            line_span.push(span);

            tokens.push(Token {
                span,
                tval: TokenVal::Terminal(Terminal::Sze),
            });

            offset_current += 3;
            left = &left[3..];
        } else if lc >= 3 && left[..3].eq_ignore_ascii_case("HLT") {
            let span = Span::new_unchecked(offset_current, offset_current + 3).line(line);
            line_span.push(span);

            tokens.push(Token {
                span,
                tval: TokenVal::Terminal(Terminal::Hlt),
            });

            offset_current += 3;
            left = &left[3..];
        } else if lc >= 3 && left[..3].eq_ignore_ascii_case("INP") {
            let span = Span::new_unchecked(offset_current, offset_current + 3).line(line);
            line_span.push(span);

            tokens.push(Token {
                span,
                tval: TokenVal::Terminal(Terminal::Inp),
            });

            offset_current += 3;
            left = &left[3..];
        } else if lc >= 3 && left[..3].eq_ignore_ascii_case("OUT") {
            let span = Span::new_unchecked(offset_current, offset_current + 3).line(line);
            line_span.push(span);

            tokens.push(Token {
                span,
                tval: TokenVal::Terminal(Terminal::Out),
            });

            offset_current += 3;
            left = &left[3..];
        } else if lc >= 3 && left[..3].eq_ignore_ascii_case("SKI") {
            let span = Span::new_unchecked(offset_current, offset_current + 3).line(line);
            line_span.push(span);

            tokens.push(Token {
                span,
                tval: TokenVal::Terminal(Terminal::Ski),
            });

            offset_current += 3;
            left = &left[3..];
        } else if lc >= 3 && left[..3].eq_ignore_ascii_case("SKO") {
            let span = Span::new_unchecked(offset_current, offset_current + 3).line(line);
            line_span.push(span);

            tokens.push(Token {
                span,
                tval: TokenVal::Terminal(Terminal::Sko),
            });

            offset_current += 3;
            left = &left[3..];
        } else if lc >= 3 && left[..3].eq_ignore_ascii_case("ION") {
            let span = Span::new_unchecked(offset_current, offset_current + 3).line(line);
            line_span.push(span);

            tokens.push(Token {
                span,
                tval: TokenVal::Terminal(Terminal::Ion),
            });

            offset_current += 3;
            left = &left[3..];
        } else if lc >= 3 && left[..3].eq_ignore_ascii_case("IOF") {
            let span = Span::new_unchecked(offset_current, offset_current + 3).line(line);
            line_span.push(span);

            tokens.push(Token {
                span,
                tval: TokenVal::Terminal(Terminal::Iof),
            });

            offset_current += 3;
            left = &left[3..];
        } else if lc >= 3 && left[..3].eq_ignore_ascii_case("ORG") {
            let span = Span::new_unchecked(offset_current, offset_current + 3).line(line);
            line_span.push(span);

            tokens.push(Token {
                span,
                tval: TokenVal::Terminal(Terminal::Org),
            });

            offset_current += 3;
            left = &left[3..];
        } else if lc >= 3 && left[..3].eq_ignore_ascii_case("HEX") {
            let span = Span::new_unchecked(offset_current, offset_current + 3).line(line);
            line_span.push(span);

            tokens.push(Token {
                span,
                tval: TokenVal::Terminal(Terminal::Hex),
            });

            offset_current += 3;
            left = &left[3..];
        } else if lc >= 3 && left[..3].eq_ignore_ascii_case("DEC") {
            let span = Span::new_unchecked(offset_current, offset_current + 3).line(line);
            line_span.push(span);

            tokens.push(Token {
                span,
                tval: TokenVal::Terminal(Terminal::Dec),
            });

            offset_current += 3;
            left = &left[3..];
        } else if (&left[0..1]).eq_ignore_ascii_case("I") {
            let span = Span::new_unchecked(offset_current, offset_current + 1).line(line);
            line_span.push(span);

            tokens.push(Token {
                span,
                tval: TokenVal::Terminal(Terminal::Indirection),
            });

            offset_current += 1;
            left = &left[1..];
        } else {
            let mut end = 1;
            loop {
                if end + offset_current >= len {
                    break;
                }
                match &left[end..=end] {
                    " " | "\t" | "," | "\r" | "\n" | ";" | "#" => break,
                    _ => end += 1,
                }
            }

            let span = Span::new_unchecked(offset_current, offset_current + end).line(line);
            line_span.push(span);

            tokens.push(Token {
                span,
                tval: TokenVal::NonTerminal,
            });

            debug!("Nonterminal with len {}: {}", end, &left[..end]);
            debug!("Broke on char {:?}", &left[end..=end]);

            offset_current += end; // TODO Might have to change this to end - 1
            left = &left[end..];
        }
    }

    info!(
        "Best token heur for this run would have been: {:.2}",
        len as f32 / tokens.len() as f32
    );
    info!(
        "Best line heur for this run would have been: {:.2}",
        len as f32 / line as f32
    );
    info!(
        "Original token Cap: {}, current Cap: {}, Grew by: {}, wasted cap: {}",
        len / TOK_HEUR,
        tokens.capacity(),
        tokens.capacity() - len / TOK_HEUR,
        tokens.capacity() - tokens.len()
    );
    info!(
        "Original line cap: {}, current cap: {}, grew by: {}, wasted cap {}",
        len / LIN_HEUR,
        lines.capacity(),
        lines.capacity() - len / LIN_HEUR,
        lines.capacity() - lines.len(),
    );

    Ok(Tokens {
        ctx: ParseContext { instr, lines },
        tokens: tokens.into_boxed_slice(),
    })
}

#[cfg(test)]
pub mod tests {
    use super::*;

    #[test]
    fn tokenizes() {
        let instr = "LAB, BSA NAME; comment text";

        let out = lex(instr).expect("Failed to tokenize at all");

        dbg!(out);
        panic!();
    }
}
