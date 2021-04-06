use super::*;
use log::debug;
use log::error;
use log::info;
use std::collections::HashMap;

use nom::{
    bytes::complete::{tag, take},
    number::complete::{le_u16, le_u32},
    IResult,
};

use crate::ass::{Span, MAGIC_VAL};
#[derive(Debug)]
pub struct ParsedReleaseFile {
    reset: u16,
    body: Box<[u16]>,
}

#[derive(Debug)]
pub struct ParsedDebugFile {
    reset: u16,
    text: Box<[u16]>,
    labels: HashMap<String, u16>,
    spans: HashMap<u16, Span>,
    src: String,
}

#[derive(Debug)]
struct DebugHeader {
    text_len: u32,
    labels_len: u32,
    spans_len: u32,
    src_len: u32,
}

fn parse_header(input: &[u8]) -> IResult<&[u8], (u16, bool)> {
    let (input, _) = tag(MAGIC_VAL)(input)?;
    let (input, reset) = le_u16(input)?;
    let (input, debug) = le_u16(input)?;

    Ok((input, (reset, debug > 0)))
}

fn parse_debug_header(input: &[u8]) -> IResult<&[u8], DebugHeader> {
    let (input, text_len) = le_u32(input)?;
    let (input, labels_len) = le_u32(input)?;
    let (input, spans_len) = le_u32(input)?;
    let (input, src_len) = le_u32(input)?;

    Ok((
        input,
        DebugHeader {
            text_len,
            labels_len,
            spans_len,
            src_len,
        },
    ))
}

fn parse_labels(input: &[u8], len: usize) -> IResult<&[u8], HashMap<String, u16>> {
    let mut out = HashMap::new();
    let mut taken = 0;
    let mut input = input;

    while taken < len {
        let (rest, location) = le_u16(input)?;
        let (rest, len) = le_u16(rest)?;
        let (rest, name) = take(len)(rest)?;

        let name: String = String::from_utf8_lossy(name).to_string();
        out.insert(name, location);

        input = rest;
        taken += len as usize + 4;
    }

    Ok((input, out))
}

fn parse_spans(input: &[u8], len: usize) -> IResult<&[u8], HashMap<u16, Span>> {
    let mut out = HashMap::new();
    let mut taken = 0;
    let mut input = input;

    while taken < len {
        let (rest, location) = le_u16(input)?;
        let (rest, _) = tag(&[0xDE, 0xAD])(rest)?;
        let (rest, start) = le_u32(rest)?;
        let (rest, end) = le_u32(rest)?;

        let span = Span::new_unchecked(start as usize, end as usize);

        out.insert(location, span);
        taken += 12;
        input = rest;
    }

    Ok((input, out))
}

/// Parse some bytes as either a release or debug build mano file
pub fn parse_either(file: &[u8]) -> Result<ParsedManoFile, ManoFileError> {
    info!("Parsing file as either debug or release build");

    let (mut rest, (reset, dbg)) = parse_header(file).map_err(|_| MFEType::NotManoFile)?;

    info!(
        "Found header with reset vector {} for {} file",
        reset,
        if dbg { "dbg" } else { "release" }
    );

    if dbg {
        let (rest, dbg_header) =
            parse_debug_header(rest).map_err(|_| MFEType::MissingDebugHeader)?;

        info!("Found debug header {:#?}", dbg_header);

        let (rest, text) = {
            let (rest, mut text) =
                take::<_, &[u8], nom::error::Error<&[u8]>>(dbg_header.text_len)(rest)
                    .map_err(|_| MFEType::FileNotLongEnough)?;

            let mut out = Vec::with_capacity(text.len() / 2);

            while text.len() > 0 {
                let (re, val) = le_u16::<_, nom::error::Error<&[u8]>>(text)
                    .map_err(|_| MFEType::FileNotLongEnough)?;
                out.push(val);
                text = re;
            }

            (rest, out.into_boxed_slice())
        };

        info!("Sucessfully took program text");

        let (rest, labels) = parse_labels(rest, dbg_header.labels_len as usize)
            .map_err(|_| MFEType::FileNotLongEnough)?;

        info!("Sucessfully got labels");
        debug!("{:#?}", labels);

        let (rest, spans) = parse_spans(rest, dbg_header.spans_len as usize)
            .map_err(|_| MFEType::FileNotLongEnough)?;

        info!("Sucessfully got spans");
        debug!("{:#?}", spans);

        if rest.len() != dbg_header.src_len as usize {
            error!("The length of the rest of the file was not enough for the source");
            return Err(MFEType::FileNotLongEnough).map_err(ManoFileError::from);
        }

        let src: String = String::from_utf8_lossy(rest).to_string();

        Ok(ParsedManoFile::Debug(ParsedDebugFile {
            reset,
            text,
            labels,
            spans,
            src,
        }))
    } else {
        let mut body = Vec::with_capacity(rest.len() / 2);

        while rest.len() > 0 {
            let (re, val) = le_u16::<_, nom::error::Error<&[u8]>>(rest)
                .map_err(|_| MFEType::FileNotLongEnough)?;

            body.push(val);
            rest = re;
        }

        let body = body.into_boxed_slice();

        Ok(ParsedManoFile::Release(ParsedReleaseFile { reset, body }))
    }
}
