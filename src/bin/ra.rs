extern crate console;
extern crate log;
extern crate pretty_env_logger;
extern crate structopt;

use std::path::PathBuf;

use console::{Color, Style};
use log::info;
use structopt::StructOpt;

#[derive(StructOpt)]
struct Args {
    file: PathBuf,

    #[structopt(short)]
    out: Option<PathBuf>,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    pretty_env_logger::init();

    let info_style = Style::new().cyan();

    println!(
        "{}",
        info_style.apply_to(format!(
            "Starting {} version {}",
            env!("CARGO_PKG_NAME"),
            env!("CARGO_PKG_VERSION")
        ))
    );

    let args = Args::from_args();

    let infile = args.file;
    let outfile = args.out.unwrap_or_else(|| infile.clone());

    println!(
        "{} {}",
        info_style.apply_to("Assembling file: "),
        infile.display()
    );
    println!(
        "{} {}",
        info_style.apply_to("Writing out to:  "),
        outfile.display()
    );

    info!(
        "Assembling {} and writing to {}",
        infile.display(),
        outfile.display()
    );

    Ok(())
}
