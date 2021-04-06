//! Parse mano files (The ouput of either [`crate::ass::release_build`] or
//! [`crate::ass::debug_build`]

mod parse;
pub use parse::*;

use std::{io::Read, path::Path};

use crate::ass::HeadlineError;

#[derive(Debug)]
pub enum ParsedManoFile {
    Release(ParsedReleaseFile),
    Debug(ParsedDebugFile),
}

impl PartialEq for ParsedManoFile {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Release(_), Self::Release(_)) => true,
            (Self::Debug(_), Self::Debug(_)) => true,
            _ => false,
        }
    }
}

#[derive(Debug)]
/// Possible errors from parsing a file as a rano bin
pub struct ManoFileError {
    file_name: Option<String>,
    ty: MFEType,
}

impl ManoFileError {
    pub fn attach_file(mut self, filename: String) -> Self {
        self.file_name.replace(filename);
        self
    }

    pub fn type_ref(&self) -> &MFEType {
        &self.ty
    }
}

impl From<std::io::Error> for ManoFileError {
    fn from(e: std::io::Error) -> Self {
        Self {
            file_name: None,
            ty: MFEType::IoError(e),
        }
    }
}

impl From<MFEType> for ManoFileError {
    fn from(e: MFEType) -> Self {
        Self {
            file_name: None,
            ty: e,
        }
    }
}

#[derive(Debug)]
/// Different types of error enoucntered while parsing a file as a rano bin
///
/// See [`ManoFileError::type_ref`]
pub enum MFEType {
    /// The magic value rano was missing from the start of the given file.
    NotManoFile,

    /// The file declared itself as a debug build but did not contain a debug header
    MissingDebugHeader,

    /// The file declared itself to contain sections for which there was no room
    FileNotLongEnough,

    /// Could not open the file for some reason or another
    IoError(std::io::Error),
}

impl HeadlineError for ManoFileError {
    fn headline(&self) -> String {
        let filename = move || {
            if let Some(filename) = &self.file_name {
                format!("while reading file {}", filename)
            } else {
                format!("")
            }
        };

        match &self.ty {
            MFEType::IoError(_) => format!("Io Error encountered {}", filename()),
            MFEType::NotManoFile => format!("File is not a rano binary file"),
            MFEType::MissingDebugHeader => format!(
                "The file declared itself to be a debug build, but did not contain a debug header"
            ),
            MFEType::FileNotLongEnough => {
                format!("The file declared itself to contain sections for which there was no room")
            }
        }
    }

    fn body(&self) -> String {
        let filename = move || {
            if let Some(filename) = &self.file_name {
                format!("while reading file {}", filename)
            } else {
                format!("")
            }
        };

        match &self.ty {
            MFEType::IoError(e) => format!("{}", e),
            _ => filename(),
        }
    }
}

/// Parse a file as either a release or debug mano file
pub fn parse_file(file: impl AsRef<Path>) -> Result<ParsedManoFile, ManoFileError> {
    let file = file.as_ref();

    let mut f = std::fs::File::open(file)
        .map_err(ManoFileError::from)
        .map_err(|e| e.attach_file(format!("{}", file.display())))?;

    let mut bytes = Vec::new();
    f.read_to_end(&mut bytes)
        .map_err(ManoFileError::from)
        .map_err(|e| e.attach_file(format!("{}", file.display())))?;

    parse_either(&bytes).map_err(|e| e.attach_file(format!("{}", file.display())))
}
