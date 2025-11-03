use thiserror::Error;

/// Error type for functions in this module.
#[derive(Debug, Error)]
pub enum ChessError {
    #[error("the supplied square index ({0}, {1}) was invalid")]
    InvalidSquareIndex(usize, usize),

    #[error("invalid UCI notation: {0}")]
    InvalidUciNotation(String),

    #[error("invalid file: {0}")]
    InvalidFile(char),

    #[error("invalid rank: {0}")]
    InvalidRank(char),
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

/// A type represeting a move, in UCI format. A little unsafe if you construct it manually.
#[derive(Debug, Eq, PartialEq)]
pub struct UciMove {
    /// Index of the source square in the order (rank, file).
    pub source: (usize, usize),

    /// Index of the source square in the order (rank, file).
    pub dest: (usize, usize),
}

impl UciMove {
    pub fn new(notation: &str) -> ChessResult<Self> {
        if notation.len() != 4 {
            return Err(ChessError::InvalidUciNotation(notation.to_string()));
        }

        let mut iter = notation.chars();
        let invalid_err = || ChessError::InvalidUciNotation(notation.to_string());

        let source_file = Self::file_to_index(iter.next().ok_or_else(invalid_err)?)?;
        let source_rank = Self::rank_to_index(iter.next().ok_or_else(invalid_err)?)?;
        let dest_file = Self::file_to_index(iter.next().ok_or_else(invalid_err)?)?;
        let dest_rank = Self::rank_to_index(iter.next().ok_or_else(invalid_err)?)?;

        Ok(Self {
            source: (source_rank, source_file),
            dest: (dest_rank, dest_file),
        })
    }

    /// Convert an algebraic file from a character ('a' to 'h') to an index (0 to 7).
    pub fn file_to_index(file: char) -> ChessResult<usize> {
        match file {
            'a' => Ok(0),
            'b' => Ok(1),
            'c' => Ok(2),
            'd' => Ok(3),
            'e' => Ok(4),
            'f' => Ok(5),
            'g' => Ok(6),
            'h' => Ok(7),
            _ => Err(ChessError::InvalidFile(file)),
        }
    }

    /// Convert an algebraic rank from a character ('1' to '8') to an index (0 to 7).
    pub fn rank_to_index(rank: char) -> ChessResult<usize> {
        match rank {
            '1' => Ok(0),
            '2' => Ok(1),
            '3' => Ok(2),
            '4' => Ok(3),
            '5' => Ok(4),
            '6' => Ok(5),
            '7' => Ok(6),
            '8' => Ok(7),
            _ => Err(ChessError::InvalidRank(rank)),
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
    pub fn at(&self, (rank, file): (usize, usize)) -> ChessResult<Piece> {
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
            WhitePawn, WhitePawn, WhitePawn, WhitePawn, WhitePawn, WhitePawn, WhitePawn, WhitePawn,
            EmptySquare, EmptySquare, EmptySquare, EmptySquare, EmptySquare, EmptySquare, EmptySquare, EmptySquare,
            EmptySquare, EmptySquare, EmptySquare, EmptySquare, EmptySquare, EmptySquare, EmptySquare, EmptySquare,
            EmptySquare, EmptySquare, EmptySquare, EmptySquare, EmptySquare, EmptySquare, EmptySquare, EmptySquare,
            EmptySquare, EmptySquare, EmptySquare, EmptySquare, EmptySquare, EmptySquare, EmptySquare, EmptySquare,
            BlackPawn, BlackPawn, BlackPawn, BlackPawn, BlackPawn, BlackPawn, BlackPawn, BlackPawn,
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
        assert_eq!(pos.at((0, 0))?, WhiteRook);
        assert_eq!(pos.at((0, 1))?, WhiteKnight);
        assert_eq!(pos.at((0, 2))?, WhiteBishop);
        assert_eq!(pos.at((0, 3))?, WhiteQueen);
        assert_eq!(pos.at((0, 4))?, WhiteKing);
        assert_eq!(pos.at((0, 5))?, WhiteBishop);
        assert_eq!(pos.at((0, 6))?, WhiteKnight);
        assert_eq!(pos.at((0, 7))?, WhiteRook);

        // Check the black pieces.
        assert_eq!(pos.at((7, 0))?, BlackRook);
        assert_eq!(pos.at((7, 1))?, BlackKnight);
        assert_eq!(pos.at((7, 2))?, BlackBishop);
        assert_eq!(pos.at((7, 3))?, BlackQueen);
        assert_eq!(pos.at((7, 4))?, BlackKing);
        assert_eq!(pos.at((7, 5))?, BlackBishop);
        assert_eq!(pos.at((7, 6))?, BlackKnight);
        assert_eq!(pos.at((7, 7))?, BlackRook);

        // Check the white and black pawns.
        for file in 0..8 {
            assert_eq!(pos.at((1, file))?, WhitePawn);
            assert_eq!(pos.at((6, file))?, BlackPawn);
        }

        // Check the empty squares, skipping ranks 0,1,6 and 7 (which have pawns and pieces on).
        for rank in 2..6 {
            for file in 0..8 {
                assert_eq!(pos.at((rank, file))?, EmptySquare);
            }
        }

        Ok(())
    }

    // Test that UCI moves can be parsed, and that the resulting index is correct.
    #[test]
    fn test_uci_move_construction() -> ChessResult<()> {
        use Piece::*;

        // Simple test move (e2-e4).
        let pos = Position::default();
        let mov = UciMove::new("e2e4")?;

        assert_eq!(pos.at(mov.source)?, WhitePawn);
        assert_eq!(pos.at(mov.dest)?, EmptySquare);

        // Check invalid notation results in errors.
        assert!(UciMove::new("").is_err());
        assert!(UciMove::new("blargle").is_err());
        assert!(UciMove::new("1214").is_err());
        assert!(UciMove::new("e2e9").is_err());
        assert!(UciMove::new("test").is_err());
        
        Ok(())
    }
}
