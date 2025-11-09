use crate::chess::{Position, UciMove};

/// From the given position, choose a move to make.
pub fn evaluate_position(pos: &Position) -> UciMove {
    let source = (6, 4);
    let dest = (4, 4);

    UciMove {
        source,
        dest,
    }
}
