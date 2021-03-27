use super::*;
use resolve::Resolved;

/// Write a u16 to a slice of u8 (little endian)
fn write_to(write: u16, to: &mut [u8]) {
    assert!(
        to.len() == 2,
        "Cannot write region larger than the size of u16"
    );
    to[0] = (write & 0x00FF) as u8;
    to[1] = ((write & 0xFF00) >> 8) as u8;
}

pub fn release(
    Resolved {
        ctx: _,
        references: _,
        hunks,
    }: Resolved,
) -> Result<Box<[u8]>> {
    info!("Beginning release step.");

    let hunkc = hunks.len();
    let len = hunks[hunkc - 1].org + hunks[hunkc - 1].extent;

    let cap = len as usize * 2 + HEADER_LEN;
    let mut out = Vec::with_capacity(cap);
    out.resize(cap, 0x00);
    let mut out = out.into_boxed_slice();

    for (idx, b) in MAGIC_VAL.iter().enumerate() {
        out[idx] = *b;
    }
    // defaults to 0
    // out[8] = false as u8;

    for hunk in hunks.into_iter() {
        let base = hunk.org;
        for (offset, (instr, _)) in hunk.instructions.into_iter().enumerate() {
            let st = (base as usize + offset) * 2 + HEADER_LEN;
            write_to(*instr, &mut out[st..st + 2]);
        }
    }

    info!("Finished release step.");

    Ok(out)
}
