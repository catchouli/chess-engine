pub mod chess;
pub mod engine;

use std::error::Error;

use chess::*;

fn main() -> Result<(), Box<dyn Error>> {
    // Initialise logging.
    tracing_subscriber::fmt::init();

    // Construct default position.
    let position = Position::default();

    //// Construct debug position.
    //let position = Position::from_visual(
    //    "
    //    rnbqkbnr
    //    .pp.pppp
    //    ........
    //    ........
    //    p.......
    //    .n......
    //    .PP..PPP
    //    RNB.KBNR
    //    ", Color::Black, None, true, true, true, true)?;

    // Play a two-player game.
    //play_two_player_game(position)?;

    // Play a one-player game.
    play_one_player_game(position, Color::White)?;

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

/// Play a two player game, where the user inputs moves for both sides.
fn play_two_player_game(initial_pos: Position) -> Result<(), Box<dyn Error>> {
    let mut pos = initial_pos;

    loop {
        println!("---\n");
        println!("Current position:\n");
        println!("{pos:#?}");

        // Get move input.
        print!("{:?} move: ", pos.side_to_move);
        let move_input: String = text_io::read!("{}\n");

        // Parse it to UCI move, checking for any errors.
        let mov = UciMove::new(&move_input);
        match mov {
            Err(_) => {
                println!("Invalid move: {move_input}");
                continue;
            },
            _ => {}
        }
        let mov = mov.unwrap();

        // Make move, checking for errors.
        match pos.make_move(mov) {
            Ok(new_pos) => {
                pos = new_pos;
            },
            Err(e) => {
                println!("Invalid move: {e}");
            }
        }
    }
}

/// Play a one player game, where the user inputs moves for only the given side.
fn play_one_player_game(initial_pos: Position, user_side: Color) -> Result<(), Box<dyn Error>> {
    let mut pos = initial_pos;

    loop {
        println!("---\n");
        println!("Current position:\n");
        println!("{pos:#?}");

        // If this is the user side, get their input.
        if pos.side_to_move == user_side {
            // Get move input.
            print!("{:?} move: ", pos.side_to_move);
            let move_input: String = text_io::read!("{}\n");

            // Parse it to UCI move, checking for any errors.
            let mov = UciMove::new(&move_input);
            match mov {
                Err(_) => {
                    println!("Invalid move: {move_input}");
                    continue;
                },
                _ => {}
            }
            let mov = mov.unwrap();

            // Make move, checking for errors.
            match pos.make_move(mov) {
                Ok(new_pos) => {
                    pos = new_pos;
                },
                Err(e) => {
                    println!("Invalid move: {e}");
                }
            }
        }
        else {
            println!("{:?} move (computer)...", pos.side_to_move);

            let computer_move = engine::evaluate_position(&pos)?;

            pos= pos.make_move(computer_move)
                .expect("Computer made invalid move");
        }
    }
}
