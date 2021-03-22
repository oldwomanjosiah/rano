extern crate console;
extern crate log;
extern crate pretty_env_logger;
extern crate structopt;

use std::{fs::read_to_string, io::Write, path::PathBuf};

use console::{Color, Style};
use log::{error, info};
// use rano::{ass::assemble_debug, assemble_stripped};
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
    let mut outfile: PathBuf = args.out.unwrap_or_else(|| infile.clone());
    outfile.set_extension("hex");

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

    let instr = std::fs::read_to_string(&infile)?;

    let mut out_file = std::fs::File::create(&outfile)?;

    use rano::ass2;

    let out = ass2::lex(&instr).and_then(ass2::parse);

    match out {
        Ok(a) => eprintln!("{:#?}", a),
        Err(e) => {
            eprintln!("{:#?}", e);
            eprintln!("{}", e);
        }
    }
    /*
    let buffer = match assemble_debug(&instr) {
        Ok(b) => b,
        Err(e) => {
            error!("Error encountered:\n {}", e);
            eprintln!("{}", e);
            std::process::exit(-1);
        }
    };

    out_file.write_all(buffer.buffer())?;
    */

    Ok(())
}
