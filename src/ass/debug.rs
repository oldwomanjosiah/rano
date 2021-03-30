//! Assemble a debug build
//!
//! TODO: Add file layout information
use super::*;
use log::info;
use resolve::Resolved;

const BYTES_P_SPAN: usize = 12;
const BYTES_P_INSTR: usize = 2;

/// Represents the amount of bytes the label reference header takes up
/// (2 for address, 2 for name len)
const REF_HEAD_LEN: usize = 4;

/// Represents the amount of bytes taken up by the debug header.
/// (4 for: text len, labels len, spans len, src len)
const DEB_HEAD_LEN: usize = 4 * 4;

const REF_LEN_HEUR: usize = 6;

/// Write copy type T to a mutable slice of the size as T (e.g. a u16 must have `at.len() == 2`)
fn write_to<'a, 'b, T: Copy>(t: &'a T, at: &'b mut [u8]) {
    let len = std::mem::size_of::<T>();

    debug_assert!(
        at.len() == len,
        "Cannot write type {} of size {} to buf of length {}",
        std::any::type_name::<T>(),
        len,
        at.len()
    );

    // Data is valid across all reads within 0..`len`
    // Data is properly aligned for u8 (u8 alignment is 1)
    // Data cannot be mutated while we hold this as its lifetime ends before the borrow of t
    //
    // https://doc.rust-lang.org/std/slice/fn.from_raw_parts.html#safety
    let bytes: &'a [u8] =
        unsafe { std::slice::from_raw_parts::<'a>(t as *const T as *const _, len) };

    for (idx, &i) in bytes.iter().enumerate() {
        at[idx] = i;
    }
}

fn build_ref_sec(refs: &HashMap<&str, (u16, Span)>) -> Box<[u8]> {
    let starting_cap = refs.len() * (REF_LEN_HEUR + REF_HEAD_LEN);
    let mut out = Vec::with_capacity(starting_cap);

    info!(
        "Building reference section with a reference len heur of {}",
        REF_LEN_HEUR
    );

    for (&lab, &(loc, _)) in refs.into_iter() {
        let offset = out.len();

        out.resize(offset + REF_HEAD_LEN + lab.len(), 0);

        let window = &mut out[offset..];

        write_to(&(loc), &mut window[0..2]);
        write_to(&(lab.len() as u16), &mut window[2..4]);

        for (idx, i) in lab.bytes().enumerate() {
            window[4 + idx] = i;
        }
    }

    info!("Build reference section.");
    info!(
        "Best heur would have been {}. Starting cap {}. Final Cap: {}. Grew by: {}. Wasted cap {}",
        out.len() as f32 / refs.len() as f32 - REF_HEAD_LEN as f32,
        starting_cap,
        out.capacity(),
        out.capacity() - &starting_cap,
        out.capacity() - out.len()
    );

    out.shrink_to_fit();
    out.into_boxed_slice()
}

pub fn debug(
    Resolved {
        ctx,
        references,
        reset,
        hunks,
    }: Resolved,
) -> Result<Box<[u8]>> {
    info!("Beginning laying out debug file.");
    let instr_ct: usize = hunks.iter().map(|a| a.instructions.len()).sum();
    let last_instr_addr: usize = hunks
        .last()
        .map(|a| a.instructions.len() + a.org as usize)
        .expect("Getting last hunk");

    let refs_section = build_ref_sec(&references);

    let text_offset = HEADER_LEN + DEB_HEAD_LEN;
    let text_len = last_instr_addr * BYTES_P_INSTR;

    let refs_offset = text_offset + text_len;
    let refs_len = refs_section.len();

    let spans_offset = refs_offset + refs_len;
    let spans_len = instr_ct * BYTES_P_SPAN;

    let src_offset = spans_offset + spans_len;
    let src_len = ctx.instr.len();

    dbg!(text_len, text_offset);
    dbg!(refs_len, refs_offset);
    dbg!(spans_len, spans_offset);
    dbg!(src_len, src_offset);

    let total_len = HEADER_LEN + DEB_HEAD_LEN + text_len + refs_len + spans_len + src_len;

    dbg!(total_len);

    let mut out = Vec::with_capacity(total_len);
    out.resize(total_len, 0);
    let mut out = out.into_boxed_slice();

    // Insert Header
    {
        let window = &mut out[0..HEADER_LEN];

        for (idx, &b) in MAGIC_VAL.iter().enumerate() {
            window[idx] = b;
        }

        write_to(&reset, &mut window[4..6]);
        window[7] = true as u8;
    }

    // Insert Debug Header
    {
        let window = &mut out[HEADER_LEN..HEADER_LEN + DEB_HEAD_LEN];

        write_to(&(text_len as u32), &mut window[0..4]);
        write_to(&(refs_len as u32), &mut window[4..8]);
        write_to(&(spans_len as u32), &mut window[8..12]);
        write_to(&(src_len as u32), &mut window[12..16]);
    }
    // Insert Program Text
    {
        let window = &mut out[text_offset..text_offset + text_len];

        for hunk in hunks.iter() {
            let base = hunk.org as usize * BYTES_P_INSTR;
            for (offset, (instr, _)) in hunk.instructions.iter().enumerate() {
                let st = base + offset * BYTES_P_INSTR;
                write_to(instr, &mut window[st..st + BYTES_P_INSTR]);
            }
        }
    }
    // Insert Labels
    {
        let window = &mut out[refs_offset..refs_offset + refs_len];

        for (idx, &b) in refs_section.into_iter().enumerate() {
            window[idx] = b;
        }
    }
    // Insert Spans
    {
        let mut window = &mut out[spans_offset..spans_offset + spans_len];

        for hunk in hunks.into_iter() {
            let base = hunk.org;
            for (offset, &(_, span)) in hunk.instructions.into_iter().enumerate() {
                // Address
                write_to(&(base + offset as u16), &mut window[0..2]);
                // Padding that looks different than 0x00
                window[2] = 0xDE;
                window[3] = 0xAD;
                // Span start
                write_to(&(span.char_st as u32), &mut window[4..8]);
                // Span end
                write_to(&(span.char_en as u32), &mut window[8..12]);

                window = &mut window[BYTES_P_SPAN..];
            }
        }
    }
    // Insert Src
    {
        let window = &mut out[src_offset..src_offset + src_len];

        for (idx, b) in ctx.instr.bytes().enumerate() {
            window[idx] = b;
        }
    }

    info!("Finished laying out debug file.");
    Ok(out)
}
