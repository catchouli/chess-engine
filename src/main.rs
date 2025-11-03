pub mod chess;

use chess::Position;

fn main() {
    // Initialise logging.
    tracing_subscriber::fmt::init();

    // Construct default position.
    let position = Position::default();

    println!("Current position:\n");
    println!("{position:#?}");
    println!("---");

    // Get an invalid index.
    match position.at(0, 0) {
        Ok(piece) => log::info!("Got piece: {piece:?}"),
        Err(e) => log::info!("Got error when accessing piece: {e}"),
    }
}
