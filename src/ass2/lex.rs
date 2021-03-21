use super::*;
use log::info;

// Represents the divisor for chars used to initalize tokens vec
const HEUR: usize = 4;

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
    pub instr: &'a str,
    pub tokens: Box<[Token]>,
}

pub fn lex(instr: &str) -> Result<Tokens> {
    let len = instr.len();

    let mut offset_current = 0;
    let mut left = &instr[..];

    let mut tokens = Vec::with_capacity(len / HEUR);

    let mut line = 1;

    while offset_current < len {
        let lc = len - offset_current;

        if &left[0..1] == " " || &left[0..1] == "\t" {
            offset_current += 1;
            left = &left[1..];
        } else if &left[0..1] == "\n" {
            tokens.push(Token {
                span: Span::new_unchecked(offset_current, offset_current + 1).line(line),
                tval: TokenVal::Terminal(Terminal::Newline),
            });

            offset_current += 1;
            left = &left[1..];
            line += 1;
        } else if &left[0..2] == "\r\n" {
            tokens.push(Token {
                span: Span::new_unchecked(offset_current, offset_current + 2).line(line),
                tval: TokenVal::Terminal(Terminal::Newline),
            });

            offset_current += 2;
            left = &left[2..];
            line += 1;
        } else if &left[0..1] == "," {
            tokens.push(Token {
                span: Span::new_unchecked(offset_current, offset_current + 1).line(line),
                tval: TokenVal::Terminal(Terminal::Comma),
            });

            offset_current += 1;
            left = &left[1..];
        } else if &left[0..1] == ";" || &left[0..1] == "#" {
            tokens.push(Token {
                span: Span::new_unchecked(offset_current, offset_current + 1).line(line),
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
            tokens.push(Token {
                span: Span::new_unchecked(offset_current, offset_current + end).line(line),
                tval: TokenVal::Ignored,
            });

            info!("Ignored comment text with len {}: {}", end, &left[..end]);
            info!("Broke on char '{}'", &left[end..=end]);

            offset_current += end; // TODO Might have to change this to end - 1
            left = &left[end..];
        } else if lc >= 3 && left[..3].eq_ignore_ascii_case("AND") {
            tokens.push(Token {
                span: Span::new_unchecked(offset_current, offset_current + 3).line(line),
                tval: TokenVal::Terminal(Terminal::And),
            });

            offset_current += 3;
            left = &left[3..];
        } else if lc >= 3 && left[..3].eq_ignore_ascii_case("ADD") {
            tokens.push(Token {
                span: Span::new_unchecked(offset_current, offset_current + 3).line(line),
                tval: TokenVal::Terminal(Terminal::Add),
            });

            offset_current += 3;
            left = &left[3..];
        } else if lc >= 3 && left[..3].eq_ignore_ascii_case("LDA") {
            tokens.push(Token {
                span: Span::new_unchecked(offset_current, offset_current + 3).line(line),
                tval: TokenVal::Terminal(Terminal::Lda),
            });

            offset_current += 3;
            left = &left[3..];
        } else if lc >= 3 && left[..3].eq_ignore_ascii_case("STA") {
            tokens.push(Token {
                span: Span::new_unchecked(offset_current, offset_current + 3).line(line),
                tval: TokenVal::Terminal(Terminal::Sta),
            });

            offset_current += 3;
            left = &left[3..];
        } else if lc >= 3 && left[..3].eq_ignore_ascii_case("BUN") {
            tokens.push(Token {
                span: Span::new_unchecked(offset_current, offset_current + 3).line(line),
                tval: TokenVal::Terminal(Terminal::Bun),
            });

            offset_current += 3;
            left = &left[3..];
        } else if lc >= 3 && left[..3].eq_ignore_ascii_case("BSA") {
            tokens.push(Token {
                span: Span::new_unchecked(offset_current, offset_current + 3).line(line),
                tval: TokenVal::Terminal(Terminal::Bsa),
            });

            offset_current += 3;
            left = &left[3..];
        } else if lc >= 3 && left[..3].eq_ignore_ascii_case("ISZ") {
            tokens.push(Token {
                span: Span::new_unchecked(offset_current, offset_current + 3).line(line),
                tval: TokenVal::Terminal(Terminal::Isz),
            });

            offset_current += 3;
            left = &left[3..];
        } else if lc >= 3 && left[..3].eq_ignore_ascii_case("CLA") {
            tokens.push(Token {
                span: Span::new_unchecked(offset_current, offset_current + 3).line(line),
                tval: TokenVal::Terminal(Terminal::Cla),
            });

            offset_current += 3;
            left = &left[3..];
        } else if lc >= 3 && left[..3].eq_ignore_ascii_case("CLE") {
            tokens.push(Token {
                span: Span::new_unchecked(offset_current, offset_current + 3).line(line),
                tval: TokenVal::Terminal(Terminal::Cle),
            });

            offset_current += 3;
            left = &left[3..];
        } else if lc >= 3 && left[..3].eq_ignore_ascii_case("CMA") {
            tokens.push(Token {
                span: Span::new_unchecked(offset_current, offset_current + 3).line(line),
                tval: TokenVal::Terminal(Terminal::Cma),
            });

            offset_current += 3;
            left = &left[3..];
        } else if lc >= 3 && left[..3].eq_ignore_ascii_case("CME") {
            tokens.push(Token {
                span: Span::new_unchecked(offset_current, offset_current + 3).line(line),
                tval: TokenVal::Terminal(Terminal::Cme),
            });

            offset_current += 3;
            left = &left[3..];
        } else if lc >= 3 && left[..3].eq_ignore_ascii_case("CIR") {
            tokens.push(Token {
                span: Span::new_unchecked(offset_current, offset_current + 3).line(line),
                tval: TokenVal::Terminal(Terminal::Cir),
            });

            offset_current += 3;
            left = &left[3..];
        } else if lc >= 3 && left[..3].eq_ignore_ascii_case("CIL") {
            tokens.push(Token {
                span: Span::new_unchecked(offset_current, offset_current + 3).line(line),
                tval: TokenVal::Terminal(Terminal::Cil),
            });

            offset_current += 3;
            left = &left[3..];
        } else if lc >= 3 && left[..3].eq_ignore_ascii_case("INC") {
            tokens.push(Token {
                span: Span::new_unchecked(offset_current, offset_current + 3).line(line),
                tval: TokenVal::Terminal(Terminal::Inc),
            });

            offset_current += 3;
            left = &left[3..];
        } else if lc >= 3 && left[..3].eq_ignore_ascii_case("SPA") {
            tokens.push(Token {
                span: Span::new_unchecked(offset_current, offset_current + 3).line(line),
                tval: TokenVal::Terminal(Terminal::Spa),
            });

            offset_current += 3;
            left = &left[3..];
        } else if lc >= 3 && left[..3].eq_ignore_ascii_case("SNA") {
            tokens.push(Token {
                span: Span::new_unchecked(offset_current, offset_current + 3).line(line),
                tval: TokenVal::Terminal(Terminal::Sna),
            });

            offset_current += 3;
            left = &left[3..];
        } else if lc >= 3 && left[..3].eq_ignore_ascii_case("SZE") {
            tokens.push(Token {
                span: Span::new_unchecked(offset_current, offset_current + 3).line(line),
                tval: TokenVal::Terminal(Terminal::Sze),
            });

            offset_current += 3;
            left = &left[3..];
        } else if lc >= 3 && left[..3].eq_ignore_ascii_case("HLT") {
            tokens.push(Token {
                span: Span::new_unchecked(offset_current, offset_current + 3).line(line),
                tval: TokenVal::Terminal(Terminal::Hlt),
            });

            offset_current += 3;
            left = &left[3..];
        } else if lc >= 3 && left[..3].eq_ignore_ascii_case("INP") {
            tokens.push(Token {
                span: Span::new_unchecked(offset_current, offset_current + 3).line(line),
                tval: TokenVal::Terminal(Terminal::Inp),
            });

            offset_current += 3;
            left = &left[3..];
        } else if lc >= 3 && left[..3].eq_ignore_ascii_case("OUT") {
            tokens.push(Token {
                span: Span::new_unchecked(offset_current, offset_current + 3).line(line),
                tval: TokenVal::Terminal(Terminal::Out),
            });

            offset_current += 3;
            left = &left[3..];
        } else if lc >= 3 && left[..3].eq_ignore_ascii_case("SKI") {
            tokens.push(Token {
                span: Span::new_unchecked(offset_current, offset_current + 3).line(line),
                tval: TokenVal::Terminal(Terminal::Ski),
            });

            offset_current += 3;
            left = &left[3..];
        } else if lc >= 3 && left[..3].eq_ignore_ascii_case("SKO") {
            tokens.push(Token {
                span: Span::new_unchecked(offset_current, offset_current + 3).line(line),
                tval: TokenVal::Terminal(Terminal::Sko),
            });

            offset_current += 3;
            left = &left[3..];
        } else if lc >= 3 && left[..3].eq_ignore_ascii_case("ION") {
            tokens.push(Token {
                span: Span::new_unchecked(offset_current, offset_current + 3).line(line),
                tval: TokenVal::Terminal(Terminal::Ion),
            });

            offset_current += 3;
            left = &left[3..];
        } else if lc >= 3 && left[..3].eq_ignore_ascii_case("IOF") {
            tokens.push(Token {
                span: Span::new_unchecked(offset_current, offset_current + 3).line(line),
                tval: TokenVal::Terminal(Terminal::Iof),
            });

            offset_current += 3;
            left = &left[3..];
        } else if lc >= 3 && left[..3].eq_ignore_ascii_case("ORG") {
            tokens.push(Token {
                span: Span::new_unchecked(offset_current, offset_current + 3).line(line),
                tval: TokenVal::Terminal(Terminal::Org),
            });

            offset_current += 3;
            left = &left[3..];
        } else if lc >= 3 && left[..3].eq_ignore_ascii_case("HEX") {
            tokens.push(Token {
                span: Span::new_unchecked(offset_current, offset_current + 3).line(line),
                tval: TokenVal::Terminal(Terminal::Hex),
            });

            offset_current += 3;
            left = &left[3..];
        } else if lc >= 3 && left[..3].eq_ignore_ascii_case("DEC") {
            tokens.push(Token {
                span: Span::new_unchecked(offset_current, offset_current + 3).line(line),
                tval: TokenVal::Terminal(Terminal::Dec),
            });

            offset_current += 3;
            left = &left[3..];
        } else if (&left[0..1]).eq_ignore_ascii_case("I") {
            tokens.push(Token {
                span: Span::new_unchecked(offset_current, offset_current + 1).line(line),
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

            tokens.push(Token {
                span: Span::new_unchecked(offset_current, offset_current + end).line(line),
                tval: TokenVal::NonTerminal,
            });

            info!("Nonterminal with len {}: {}", end, &left[..end]);
            info!("Broke on char '{}'", &left[end..=end]);

            offset_current += end; // TODO Might have to change this to end - 1
            left = &left[end..];
        }
    }

    info!(
        "Heur used: {}, best heur for this run: {:.2}",
        HEUR,
        len as f32 / tokens.len() as f32
    );
    info!(
        "Len: {}, Cap: {}, Wasted Cap: {}",
        tokens.len(),
        tokens.capacity(),
        tokens.capacity() - tokens.len()
    );

    Ok(Tokens {
        instr,
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
