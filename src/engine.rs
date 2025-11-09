use crate::chess::{Position, UciMove, ChessResult};

use rand::prelude::*;

/// From the given position, choose a move to make.
pub fn evaluate_position(pos: &Position) -> ChessResult<UciMove> {
    let mut rng = rand::rng();

    // Get all legal moves for the position.
    let moves = pos.legal_moves()?;

    // Print all legal moves.
    println!("Legal moves:");
    for mov in &moves {
        let piece = pos.at(mov.source)?;
        println!("  [{:?}] {:?} -> {:?}", piece, mov.source, mov.dest);
    }
    println!();

    // Pick a random move.
    let index = rng.random_range(0..moves.len());
    let mov = moves[index];

    Ok(mov)
}
