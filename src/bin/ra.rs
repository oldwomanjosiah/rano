extern crate console;
extern crate log;
extern crate pretty_env_logger;
extern crate structopt;

use std::{io::Write, path::PathBuf};

use console::{style, Style};
use log::info;
// use rano::{ass::assemble_debug, assemble_stripped};
use structopt::StructOpt;

#[derive(StructOpt)]
struct Args {
    file: PathBuf,

    #[structopt(short)]
    out: Option<PathBuf>,

    #[structopt(long)]
    /// Assemble in release mode
    release: bool,

    #[structopt(short = "l", long)]
    /// Specify a label to act as the reset vector
    reset_label: Option<String>,

    #[structopt(short, long, conflicts_with = "reset")]
    /// Specify the exact memory location of the reset vector
    reset_location: Option<u16>,
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
    let mut outfile: PathBuf = args.out.unwrap_or_else(|| infile.clone());
    outfile.set_extension("hex");

    println!(
        "{} {}",
        info_style.apply_to("Assembling release build from file: "),
        infile.display()
    );

    info!(
        "Assembling {} and writing to {}",
        infile.display(),
        outfile.display()
    );

    let instr = std::fs::read_to_string(&infile)?;

    let _out_file = std::fs::File::create(&outfile)?;

    use rano::ass;

    let reset = args
        .reset_label
        .map(|l| ass::ResetVector::Label(l))
        .or(args.reset_location.map(|l| ass::ResetVector::Location(l)))
        .unwrap_or(ass::ResetVector::None);

    let out = ass::release_build(&instr, reset);

    let out = match out {
        Ok(a) => a,
        Err(e) => {
            // eprintln!("{:#?}", e);
            print!("{}", e);
            println!("{}", style("Could not Finish Assembling").red().bold());
            std::process::exit(-1);
        }
    };

    println!("{}", info_style.apply_to("Finished Assembling"));
    println!(
        "{} {}",
        info_style.apply_to("Writing out to:  "),
        outfile.display()
    );

    let mut out_file = std::fs::File::create(outfile)?;

    out_file.write_all(out.as_ref())?;

    println!("{}", info_style.apply_to("Finished Writing"));

    Ok(())
}
