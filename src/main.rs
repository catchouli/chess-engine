pub mod chess;

use std::error::Error;

use chess::*;

fn main() -> Result<(), Box<dyn Error>> {
    // Initialise logging.
    tracing_subscriber::fmt::init();

    // Construct default position.
    let position = Position::default();

    println!("Current position:\n");
    println!("{position:#?}");
    println!("---");

    // Get an invalid index.
    match position.at((0, 0)) {
        Ok(piece) => log::info!("Got piece: {piece:?}"),
        Err(e) => log::info!("Got error when accessing piece: {e}"),
    }

    let mov = UciMove::new("e2e4")?;
    log::info!("ucimove: {:?}", mov);

    let s = position.at(mov.source)?;
    let d = position.at(mov.dest)?;

    log::info!("source: {s:?}, dest: {d:?}");

    Ok(())
}
