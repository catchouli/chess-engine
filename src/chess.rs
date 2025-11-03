use thiserror::Error;

/// Error type for functions in this module.
#[derive(Debug, Error)]
pub enum ChessError {
    #[error("the supplied square index ({0}, {1}) was invalid")]
    InvalidSquareIndex(usize, usize),
}

/// Result type for functions in this module.
pub type ChessResult<T> = Result<T, ChessError>;


/// An enum for representing the color of each side.
#[repr(u8)]
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Color {
    White,
    Black,
}

/// An enum for representing chess pieces, including color. Color is included in order to
/// minimize the storage of piece + color into a u8.
#[repr(u8)]
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Piece {
    EmptySquare = 0,

    WhiteKing,
    WhiteQueen,
    WhiteKnight,
    WhiteBishop,
    WhiteRook,
    WhitePawn,

    BlackKing,
    BlackQueen,
    BlackKnight,
    BlackBishop,
    BlackRook,
    BlackPawn,
}

impl Piece {
    fn fen_fmt(&self) -> char {
        match self {
            Piece::EmptySquare => '.',

            Piece::WhiteKing => 'K',
            Piece::WhiteQueen => 'Q',
            Piece::WhiteKnight => 'N',
            Piece::WhiteBishop => 'B',
            Piece::WhiteRook => 'R',
            Piece::WhitePawn => 'P',

            Piece::BlackKing => 'k',
            Piece::BlackQueen => 'q',
            Piece::BlackKnight => 'n',
            Piece::BlackBishop => 'b',
            Piece::BlackRook => 'r',
            Piece::BlackPawn => 'p',
        }
    }
}

/// A single chess position, including the pieces as an 8x8 matrix and castling state.
#[derive(Copy, Clone, Eq, PartialEq)]
pub struct Position {
    /// Pieces in the order [a1, a2, a3, a4, ...], one rank at a time.
    pub pieces: [Piece; 64],

    /// The side to move.
    pub side_to_move: Color,

    /// The square that allows for en-passant, if any.
    pub on_passant_square: Option<usize>,

    /// Whether white can castle kingside.
    pub white_can_castle_kingside: bool,

    /// Whether white can castle queenside.
    pub white_can_castle_queenside: bool,

    /// Whether black can castle kingside.
    pub black_can_castle_kingside: bool,

    /// Whether black can castle queenside.
    pub black_can_castle_queenside: bool,
}

impl Position {
    /// Access a square by index (rank and then file, as in algebraic notation). Out of bounds
    /// indices will return an InvalidSquareIndex error.
    pub fn at(&self, rank: usize, file: usize) -> ChessResult<Piece> {
        if rank >= 8 || file >= 8 {
            Err(ChessError::InvalidSquareIndex(rank, file))
        }
        else {
            Ok(self.pieces[rank * 8 + file])
        }
    }
}

impl Default for Position {
    fn default() -> Self {
        use Piece::*;

        let pieces = [
            WhiteRook, WhiteKnight, WhiteBishop, WhiteQueen, WhiteKing, WhiteBishop, WhiteKnight, WhiteRook,
            EmptySquare, EmptySquare, EmptySquare, EmptySquare, EmptySquare, EmptySquare, EmptySquare, EmptySquare,
            EmptySquare, EmptySquare, EmptySquare, EmptySquare, EmptySquare, EmptySquare, EmptySquare, EmptySquare,
            EmptySquare, EmptySquare, EmptySquare, EmptySquare, EmptySquare, EmptySquare, EmptySquare, EmptySquare,
            EmptySquare, EmptySquare, EmptySquare, EmptySquare, EmptySquare, EmptySquare, EmptySquare, EmptySquare,
            EmptySquare, EmptySquare, EmptySquare, EmptySquare, EmptySquare, EmptySquare, EmptySquare, EmptySquare,
            EmptySquare, EmptySquare, EmptySquare, EmptySquare, EmptySquare, EmptySquare, EmptySquare, EmptySquare,
            BlackRook, BlackKnight, BlackBishop, BlackQueen, BlackKing, BlackBishop, BlackKnight, BlackRook,
        ];

        Self {
            pieces,
            side_to_move: Color::White,
            on_passant_square: None,
            white_can_castle_kingside: true,
            white_can_castle_queenside: true,
            black_can_castle_kingside: true,
            black_can_castle_queenside: true,
        }
    }
}

impl std::fmt::Debug for Position {
    /// A custom debug formatter for printing the board state. White pieces are represented by
    /// uppercase letters (K, Q, N, B, R, P) while black pieces are represented by lowercase
    /// letters.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Iterate the ranks as chunks.
        for rank in self.pieces.chunks(8).rev() {
            for piece in rank {
                write!(f, "{}", piece.fen_fmt())?;
            }
            writeln!(f, "")?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Test the default position is valid.
    #[test]
    fn default_position_is_valid() -> ChessResult<()> {
        use Piece::*;

        let pos = Position::default();

        assert_eq!(pos.side_to_move, Color::White);
        assert_eq!(pos.on_passant_square, None);
        assert_eq!(pos.white_can_castle_kingside, true);
        assert_eq!(pos.white_can_castle_queenside, true);
        assert_eq!(pos.black_can_castle_kingside, true);
        assert_eq!(pos.black_can_castle_queenside, true);

        // Check the white pieces.
        assert_eq!(pos.at(0, 0)?, WhiteRook);
        assert_eq!(pos.at(0, 1)?, WhiteKnight);
        assert_eq!(pos.at(0, 2)?, WhiteBishop);
        assert_eq!(pos.at(0, 3)?, WhiteQueen);
        assert_eq!(pos.at(0, 4)?, WhiteKing);
        assert_eq!(pos.at(0, 5)?, WhiteBishop);
        assert_eq!(pos.at(0, 6)?, WhiteKnight);
        assert_eq!(pos.at(0, 7)?, WhiteRook);

        // Check the black pieces.
        assert_eq!(pos.at(7, 0)?, BlackRook);
        assert_eq!(pos.at(7, 1)?, BlackKnight);
        assert_eq!(pos.at(7, 2)?, BlackBishop);
        assert_eq!(pos.at(7, 3)?, BlackQueen);
        assert_eq!(pos.at(7, 4)?, BlackKing);
        assert_eq!(pos.at(7, 5)?, BlackBishop);
        assert_eq!(pos.at(7, 6)?, BlackKnight);
        assert_eq!(pos.at(7, 7)?, BlackRook);

        // Check the empty squares, skipping ranks 0 and 7.
        for rank in 1..7 {
            for file in 0..8 {
                assert_eq!(pos.at(rank, file)?, EmptySquare);
            }
        }

        Ok(())
    }
}
