extern crate log;
extern crate nom;
extern crate pretty_env_logger;
extern crate rano;
extern crate structopt;
extern crate tui;

use std::path::PathBuf;

use log::info;
use structopt::StructOpt;

#[derive(StructOpt)]
struct Options {
    #[structopt(short, long)]
    /// The mano binary produced by ra
    ///
    /// Can be either a debug or release build.
    pub bin: Option<PathBuf>,

    #[structopt(long)]
    /// Do not enter debug mode even if bin is a debug build
    ///
    /// Does nothing if bin is a release build
    pub no_debug: bool,
}

fn main() {
    pretty_env_logger::init();

    info!(
        "Starting {} version {}",
        env!("CARGO_BIN_NAME"),
        env!("CARGO_PKG_VERSION")
    );

    let opts = Options::from_args();

    if let Some(bin) = opts.bin {
        let out = rano::parse::parse_file(bin);
        match out {
            Ok(f) => eprintln!("Parsed file as:\n{:#?}", f),
            Err(e) => eprintln!("Could not parse file for reason:\n {}", e),
        }
    }

    info!("Rano quit");
}
