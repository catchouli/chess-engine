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
}
