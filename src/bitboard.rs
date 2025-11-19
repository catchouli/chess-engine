use crate::chess::{ChessError, ChessResult};

// A bitboard-based implementation of the chess game state. We store the game state using 64-bit
// bitboards in order (a1, b1, c1, ..., f8, g8, h8), with one bitboard for each color-piece
// combination.
//
// We could make this representation denser by storing bitboards for the white and black pieces,
// and then bitboards for each color-independent piece, after which it's just a simple union to get
// back to the original color-dependent bitboards, at the cost of a little complexity. This might
// be worth considering at some point, but I've decided to avoid the complexity for now.
//
// https://www.chessprogramming.org/Bitboards
//
// TODO: castling rights.
pub struct Position {
    white_pawns: u64,
    white_knights: u64,
    white_bishops: u64,
    white_rooks: u64,
    white_queens: u64,
    white_king: u64,

    black_pawns: u64,
    black_knights: u64,
    black_bishops: u64,
    black_rooks: u64,
    black_queens: u64,
    black_king: u64,
}

impl Position {
    /// Create an empty board.
    fn empty() -> Position {
        Position {
            white_pawns: 0,
            white_knights: 0,
            white_bishops: 0,
            white_rooks: 0,
            white_queens: 0,
            white_king: 0,
            black_pawns: 0,
            black_knights: 0,
            black_bishops: 0,
            black_rooks: 0,
            black_queens: 0,
            black_king: 0,
        }
    }

    /// Create a new position from a visual (string) representation.
    /// For example, for the default position:
    /// "
    /// rnbqkbnr
    /// pppppppp
    /// ........
    /// ........
    /// ........
    /// ........
    /// PPPPPPPP
    /// RNBQKBNR
    /// "
    /// Whitespace and line breaks are dropped from the input, and the remaining characters must
    /// number at least 64, and be a valid piece (uppercase for white, lowercase for black, and '.'
    /// for an empty square.) To visually resemble a chess board, a8 is the 1st element, and h1 is
    /// the 64th.
    pub fn from_visual(visual: &str) -> ChessResult<Position> {
        // Create empty board.
        let mut board = Self::empty();

        // Iterate position and fill out bitboards.
        let mut idx = 0;
        for c in visual.chars() {
            // Prevent index from overflowing.
            if idx > 64 {
                break;
            }

            // Skip whitespace.
            if c == ' ' || c == '\n' || c == '\r' {
                continue;
            }

            // Invert the rank, the ranks in the visual representation start at h1 but in our
            // internal representation we start at a1.
            let file = idx % 8;
            let rank = 8 - (idx - file) / 8;
            let internal_idx = (rank - 1) * 8 + file;

            // Match the input character and use it to populate the bitboards.
            match c {
                // White pieces.
                'P' => { board.white_pawns |= 1 << internal_idx; },
                'R' => { board.white_rooks |= 1 << internal_idx; },
                'N' => { board.white_knights |= 1 << internal_idx; },
                'B' => { board.white_bishops |= 1 << internal_idx; },
                'Q' => { board.white_queens |= 1 << internal_idx; },
                'K' => { board.white_king |= 1 << internal_idx; },

                // Black pieces.
                'p' => { board.black_pawns |= 1 << internal_idx; },
                'r' => { board.black_rooks |= 1 << internal_idx; },
                'n' => { board.black_knights |= 1 << internal_idx; },
                'b' => { board.black_bishops |= 1 << internal_idx; },
                'q' => { board.black_queens |= 1 << internal_idx; },
                'k' => { board.black_king |= 1 << internal_idx; },

                // Empty square (do nothing.)
                '.' => {},

                // Invalid piece type.
                _ => return Err(ChessError::InvalidPieceNotation(c))
            }

            // Increment index.
            idx += 1;
        }

        // idx should now be the length of the input, check it was correct.
        if idx != 64 {
            return Err(ChessError::InvalidInputLength(idx, 64));
        }

        Ok(board)
    }

    /// Get the piece at a position, in letter format, uppercase for White, lowercase for Black,
    /// and '.' for empty squares.
    fn piece_at_position(&self, rank: usize, file: usize) -> char {
        // Return an empty space for out of range coordinates to keep things simple.
        if rank > 7 || file > 7 {
            return '.';
        }

        // Calculate internal index and mask.
        let idx = rank * 8 + file;
        let mask = 1 << idx;

        // Table of piece types to look up, and references to their corresponding bitboards. This
        // code would be made simpler if we used an array-of-bitboards, but this function is mainly
        // used for debug-printing, so it's not really necessary. For now, this saves us a big
        // tree of if-else-if.
        let bitboards = [
            ('P', &self.white_pawns),
            ('R', &self.white_rooks),
            ('N', &self.white_knights),
            ('B', &self.white_bishops),
            ('Q', &self.white_queens),
            ('K', &self.white_king),

            ('p', &self.black_pawns),
            ('r', &self.black_rooks),
            ('n', &self.black_knights),
            ('b', &self.black_bishops),
            ('q', &self.black_queens),
            ('k', &self.black_king),
        ];

        // Iterate bitboards to check for pieces.
        for (c, bitboard) in bitboards {
            if (bitboard & mask) != 0 {
                return c;
            }
        }

        // Finally, this must be an empty square.
        return '.';
    }
}

impl std::fmt::Debug for Position {
    /// A custom debug formatter for printing the board state. White pieces are represented by
    /// uppercase letters (K, Q, N, B, R, P) while black pieces are represented by lowercase
    /// letters.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Iterate ranks in reverse order.
        for rank in (0..8).rev() {
            // Iterate files.
            for file in 0..8 {
                let piece = self.piece_at_position(rank, file);
                write!(f, "{piece}")?;
            }
            writeln!(f)?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

}
