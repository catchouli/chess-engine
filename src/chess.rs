use thiserror::Error;

/// Error type for functions in this module.
#[derive(Debug, Error)]
pub enum ChessError {
    #[error("the supplied square index ({0}, {1}) was invalid")]
    InvalidSquareIndex(usize, usize),

    #[error("invalid UCI notation: {0}")]
    InvalidUciNotation(String),

    #[error("invalid piece notation: {0}")]
    InvalidPieceNotation(char),

    #[error("not enough pieces")]
    NotEnoughPieces,

    #[error("invalid file: {0}")]
    InvalidFile(char),

    #[error("invalid rank: {0}")]
    InvalidRank(char),

    #[error("there was no piece at the specified coordinates ({0}, {1})")]
    NoPiece(usize, usize),

    #[error("the piece doesn't belong to the active side")]
    WrongSide,

    #[error("the move was illegal")]
    IllegalMove,

    #[error("internal error: the given straight line move was not along a straight line")]
    InvalidStraightLineMove,

    #[error("internal error: the given diagonal move was not along a diagonal")]
    InvalidDiagonalMove,
}

/// Result type for functions in this module.
pub type ChessResult<T> = Result<T, ChessError>;

/// Type for representing piece indexes, (rank, file) indexed from 0 to 7. Probably the least safe
/// part of this setup, but it's the simplest way, so we just have to check it's legal when we use
/// it.
pub type SquareIndex = (usize, usize);

/// Convert SquareIndex to signed representation.
fn to_signed(idx: &SquareIndex) -> (i32, i32) {
    (idx.0 as i32, idx.1 as i32)
}

/// An enum for representing the color of each side.
#[repr(u8)]
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Color {
    White,
    Black,
}

impl Color {
    fn flip(&self) -> Self {
        match self {
            Color::White => Color::Black,
            Color::Black => Color::White
        }
    }
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
    fn from_char(c: char) -> ChessResult<Self> {
        use Piece::*;

        match c {
            '.' => Ok(EmptySquare),

            'K' => Ok(WhiteKing),
            'Q' => Ok(WhiteQueen),
            'N' => Ok(WhiteKnight),
            'B' => Ok(WhiteBishop),
            'R' => Ok(WhiteRook),
            'P' => Ok(WhitePawn),

            'k' => Ok(BlackKing),
            'q' => Ok(BlackQueen),
            'n' => Ok(BlackKnight),
            'b' => Ok(BlackBishop),
            'r' => Ok(BlackRook),
            'p' => Ok(BlackPawn),

            _ => Err(ChessError::InvalidPieceNotation(c))
        }
    }

    fn to_char(&self) -> char {
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

    fn color(&self) -> Option<Color> {
        match self {
            Piece::EmptySquare => None,

            Piece::WhiteKing => Some(Color::White),
            Piece::WhiteQueen => Some(Color::White),
            Piece::WhiteKnight => Some(Color::White),
            Piece::WhiteBishop => Some(Color::White),
            Piece::WhiteRook => Some(Color::White),
            Piece::WhitePawn => Some(Color::White),

            Piece::BlackKing => Some(Color::Black),
            Piece::BlackQueen => Some(Color::Black),
            Piece::BlackKnight => Some(Color::Black),
            Piece::BlackBishop => Some(Color::Black),
            Piece::BlackRook => Some(Color::Black),
            Piece::BlackPawn => Some(Color::Black),
        }
    }
}

/// A type represeting a move, in UCI format. A little unsafe if you construct it manually.
#[derive(Debug, Eq, PartialEq)]
pub struct UciMove {
    /// Index of the source square in the order (rank, file).
    pub source: SquareIndex,

    /// Index of the source square in the order (rank, file).
    pub dest: SquareIndex,
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
    pub en_passant_square: Option<SquareIndex>,

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
    pub fn from_visual(
        board: &str,
        side_to_move: Color,
        en_passant_square: Option<SquareIndex>,
        white_can_castle_kingside: bool,
        white_can_castle_queenside: bool,
        black_can_castle_kingside: bool,
        black_can_castle_queenside: bool,
    ) -> ChessResult<Position>
    {
        // Parse pieces to 64 Piece values.
        let pieces: Vec<Piece> = board.chars()
            // Filter out whitespace and linebreaks.
            .filter(|c| *c != ' ' && *c != '\r' && *c != '\n')
            // Convert from char to Piece.
            .map(Piece::from_char)
            // Flatten the results.
            .flatten()
            // Collect into a Vec<Piece>.
            .collect();

        // Convert the Vec<Piece> to a [Piece; 64], or an error if there aren't enough elements.
        let pieces: [Piece; 64] = pieces
            // Kinda annoying, I realised that the ranks actually need transposing as the black
            // pieces are at the top in the visual representation but our internal indexing starts
            // with the white pieces.
            .chunks(8).rev().flatten()
            .map(|p| *p)
            .collect::<Vec<_>>()
            // Convert from Vec<Piece> to [Piece; 64], returning an error if there aren't enough
            // pieces.
            .try_into()
            .map_err(|_| ChessError::NotEnoughPieces)?;

        Ok(Position {
            pieces,
            side_to_move,
            en_passant_square,
            white_can_castle_kingside,
            white_can_castle_queenside,
            black_can_castle_kingside,
            black_can_castle_queenside
        })
    }

    /// Access a square by index (rank and then file, as in algebraic notation). Out of bounds
    /// indices will return an InvalidSquareIndex error.
    pub fn at(&self, (rank, file): SquareIndex) -> ChessResult<Piece> {
        if rank >= 8 || file >= 8 {
            Err(ChessError::InvalidSquareIndex(rank, file))
        }
        else {
            Ok(self.pieces[rank * 8 + file])
        }
    }

    /// Set the piece at a given index.
    pub fn set_at(&mut self, (rank, file): SquareIndex, piece: Piece) -> ChessResult<()> {
        if rank >= 8 || file >= 8 {
            Err(ChessError::InvalidSquareIndex(rank, file))
        }
        else {
            self.pieces[rank * 8 + file] = piece;
            Ok(())
        }
    }

    /// Check whether a give move is legal.
    /// TODO: Some of the code is a bit nasty, it might be worth simplifying it if possible once
    /// it's implemented.
    /// TODO: support castling.
    /// TODO: needs test cases, but it turned out to be trickier to write than I expected, so I'm
    /// just implementing it fully first. There's probably some stuff in there that'll go wrong
    /// though without robust test cases...
    pub fn is_move_legal(&self, mov: &UciMove) -> ChessResult<bool> {
        use Piece::*;

        let source = mov.source;
        let dest = mov.dest;

        // Check index validity.
        if source.0 > 7 {
            return Err(ChessError::InvalidSquareIndex(source.0, source.1));
        }
        if source.1 > 7 {
            return Err(ChessError::InvalidSquareIndex(source.0, source.1));
        }
        if dest.0 > 7 {
            return Err(ChessError::InvalidSquareIndex(dest.0, dest.1));
        }
        if dest.1 > 7 {
            return Err(ChessError::InvalidSquareIndex(dest.0, dest.1));
        }

        // Get source piece.
        let source_piece = self.at(source)?;

        if source_piece.color() != Some(self.side_to_move) {
            return Err(ChessError::WrongSide);
        }

        // Check move validity.
        match source_piece {
            WhitePawn | BlackPawn => self.is_valid_pawn_move(source, dest),
            WhiteKnight | BlackKnight => self.is_valid_knight_move(source, dest),
            WhiteRook | BlackRook => self.is_valid_rook_move(source, dest),
            WhiteBishop | BlackBishop => self.is_valid_bishop_move(source, dest),
            WhiteQueen | BlackQueen => self.is_valid_queen_move(source, dest),
            WhiteKing | BlackKing => self.is_valid_king_move(source, dest),

            // Any other move isn't legal.
            _ => Ok(false)
        }
    }

    /// Check if the given move is a valid pawn move.
    fn is_valid_pawn_move(&self, source: SquareIndex, dest: SquareIndex) -> ChessResult<bool> {
        use Piece::*;

        // The passing rank (the one right in front of a pawn).
        let passing_rank =
            if self.side_to_move == Color::White { source.0 + 1 } else { source.0 - 1 };

        // Get the opposite side color.
        let opposite_color = self.side_to_move.flip();

        // Pawns can never move from the 1st or 8th rank. This also prevents some overflows
        // later.
        if source.0 > 0 && source.0 < 7 {
            // Check moves along the same file (regular moves).
            if source.1 == dest.1 {
                let move_dist = dest.0 as i32 - source.0 as i32;
                let relative_move_dist =
                    if self.side_to_move == Color::White { move_dist } else { -move_dist };

                // It should be possible to move one square forward, as long as the
                // destination square is unobstructed.
                if relative_move_dist == 1 && self.at(dest)? == EmptySquare {
                    // TODO: these returns are a bit ugly, this case should either be moved
                    // into its own function, or it should be refactored.
                    return Ok(true);
                }

                // It should also be possible to move two squares forward, as long as both
                // squares are unobstructed.
                if relative_move_dist == 2 {
                    let passing_square = (passing_rank, source.1);
                    if self.at(dest)? == EmptySquare
                        && self.at(passing_square)? == EmptySquare
                    {
                        return Ok(true);
                    }
                }
            }

            // Check captures to the left.
            if source.1 > 0 && dest.1 == source.1 - 1 && dest.0 == passing_rank {
                // Check that the square contains an opponent's piece.
                if self.at(dest)?.color() == Some(opposite_color)
                    || self.en_passant_square == Some(dest)
                {
                    return Ok(true);
                }
            }

            // Check captures to the right.
            if source.1 < 7 && dest.1 == source.1 + 1 && dest.0 == passing_rank {
                // Check that the square contains an opponent's piece.
                if self.at(dest)?.color() == Some(opposite_color)
                    || self.en_passant_square == Some(dest)
                {
                    return Ok(true);
                }
            }
        }

        // Otherwise, it's not a valid move.
        Ok(false)
    }

    /// Check if the given move is a valid knight move.
    fn is_valid_knight_move(&self, source: SquareIndex, dest: SquareIndex) -> ChessResult<bool> {
        use Piece::*;

        // Calculate absolute algebraic move distance (e.g. (2, 1), (1, 1), etc).
        let move_dist_abs = (dest.0.abs_diff(source.0), dest.1.abs_diff(source.1));

        // Get the destination piece.
        let dest_piece = self.at(dest)?;

        // Check if the move is the right shape.
        let is_right_shape = move_dist_abs == (2,1) || move_dist_abs == (1,2);

        // Check if the destination piece is an empty square or a piece of the opposite
        // color.
        let is_destination_clear = dest_piece == EmptySquare
            || dest_piece.color() == Some(self.side_to_move.flip());

        Ok(is_right_shape && is_destination_clear)
    }

    /// Check if the given move is a valid rook move.
    fn is_valid_rook_move(&self, source: SquareIndex, dest: SquareIndex) -> ChessResult<bool> {
        // Convert source and dest to signed numbers.
        let signed_source = to_signed(&source);
        let signed_dest = to_signed(&dest);

        // Calculate algebraic move distance (e.g. (2, 1), (1, 1), etc).
        let move_dist = (signed_dest.0 - signed_source.0, signed_dest.1 - signed_source.1);

        // One of the dimensions should be 0, or it's not a valid rook move.
        if move_dist.0 != 0 && move_dist.1 != 0 {
            return Ok(false);
        }

        // They also can't both be 0.
        if move_dist.0 == 0 && move_dist.1 == 0 {
            return Ok(false);
        }

        // Check that the destination square is either unobstructed or an enemy piece.
        let dest_piece = self.at(dest)?;
        if dest_piece != Piece::EmptySquare
            && dest_piece.color() != Some(self.side_to_move.flip())
        {
            return Ok(false)
        }

        // Check whether the line between the source and destination squares is obstructed.
        let is_line_obstructed = self.is_straight_line_obstructed(source, dest)?;

        Ok(!is_line_obstructed)
    }

    /// Check if the given move is a valid bishop move.
    fn is_valid_bishop_move(&self, source: SquareIndex, dest: SquareIndex) -> ChessResult<bool> {
        // Convert source and dest to signed numbers.
        let signed_source = to_signed(&source);
        let signed_dest = to_signed(&dest);

        // Calculate algebraic move distance (e.g. (2, 1), (1, 1), etc).
        let move_dist = (signed_dest.0 - signed_source.0, signed_dest.1 - signed_source.1);

        // Check it's a diagonal move, in which case both axes should have the same value.
        if move_dist.0.abs() != move_dist.1.abs() {
            return Ok(false);
        }

        // Check that the destination square is either unobstructed or an enemy piece.
        let dest_piece = self.at(dest)?;
        if dest_piece != Piece::EmptySquare
            && dest_piece.color() != Some(self.side_to_move.flip())
        {
            return Ok(false)
        }

        // Check whether the line between the source and destination squares is obstructed.
        let is_line_obstructed = self.is_diagonal_line_obstructed(source, dest)?;

        Ok(!is_line_obstructed)
    }

    /// Check if the given move is a valid queen move.
    fn is_valid_queen_move(&self, source: SquareIndex, dest: SquareIndex) -> ChessResult<bool> {
        return Ok(self.is_valid_bishop_move(source, dest)?
            || self.is_valid_rook_move(source, dest)?);
    }

    /// Check if the given move is a valid king move.
    fn is_valid_king_move(&self, source: SquareIndex, dest: SquareIndex) -> ChessResult<bool> {
        Ok(false)
    }

    /// Check if the given straight line (not diagonal) move is obstructed, not including the
    /// source and destination square.
    fn is_straight_line_obstructed(&self, source: SquareIndex, dest: SquareIndex) -> ChessResult<bool> {
        // Convert source and dest to signed numbers.
        let source = to_signed(&source);
        let dest = to_signed(&dest);

        // Calculate algebraic move distance (e.g. (2, 1), (1, 1), etc).
        let move_dist = (dest.0 - source.0, dest.1 - source.1);

        // Check that the overall move distance isn't 0.
        if move_dist.0 == 0 && move_dist.1 == 0 {
            return Err(ChessError::InvalidStraightLineMove);
        }

        // Check that one of the axes is 0.
        if move_dist.0 != 0 && move_dist.1 != 0 {
            return Err(ChessError::InvalidStraightLineMove);
        }

        // Calculate step.
        let step = (move_dist.0.signum(), move_dist.1.signum());

        // Step every square in between to check for obstructions.
        let mut square = (source.0 + step.0, source.1 + step.1);
        while square != dest {
            let square_index = (square.0 as usize, square.1 as usize);

            if self.at(square_index)? != Piece::EmptySquare {
                return Ok(true)
            }

            // Step to next square.
            square = (square.0 + step.0, square.1 + step.1);
        }

        Ok(false)
    }

    /// Check if the given diagonal line is obstructed, not including the source and destination square.
    fn is_diagonal_line_obstructed(&self, source: SquareIndex, dest: SquareIndex) -> ChessResult<bool> {
        // Convert source and dest to signed numbers.
        let source = to_signed(&source);
        let dest = to_signed(&dest);

        // Calculate algebraic move distance (e.g. (2, 1), (1, 1), etc).
        let move_dist = (dest.0 - source.0, dest.1 - source.1);

        // Check that the overall move distance isn't 0.
        if move_dist.0 == 0 && move_dist.1 == 0 {
            return Err(ChessError::InvalidDiagonalMove);
        }

        // Check that both axes are the same absolute value, in which case it's a diagonal move.
        if move_dist.0.abs() != move_dist.1.abs() {
            return Err(ChessError::InvalidDiagonalMove);
        }

        // Calculate step.
        let step = (move_dist.0.signum(), move_dist.1.signum());

        // Step every square in between to check for obstructions.
        let mut square = (source.0 + step.0, source.1 + step.1);
        while square != dest {
            let square_index = (square.0 as usize, square.1 as usize);

            if self.at(square_index)? != Piece::EmptySquare {
                return Ok(true)
            }

            // Step to next square.
            square = (square.0 + step.0, square.1 + step.1);
        }

        Ok(false)
    }

    /// Returns the en-passant square for a given move. Doesn't check if the move is legal, so
    /// needs to be done after is_move_legal.
    pub fn en_passant_square_for_move(&self, mov: &UciMove) -> ChessResult<Option<SquareIndex>> {
        use Piece::*;

        let source = mov.source;
        let dest = mov.dest;

        let source_piece = self.at(mov.source)?;

        // If this isn't a pawn move then there isn't an en-passant square.
        if source_piece == WhitePawn {
            // If it's a move from the second rank, on the same file, and two spaces, the
            // en-passant square is in between.
            if source.0 == 1 && source.1 == dest.1 && dest.0 > source.0 && dest.0 - source.0 == 2 {
                return Ok(Some((source.0 + 1, source.1)));
            }
        }

        if source_piece == BlackPawn {
            // If it's a move from the second rank, on the same file, and two spaces, the
            // en-passant square is in between.
            if source.0 == 6 && source.1 == dest.1 && dest.0 < source.0 && source.0 - dest.0 == 2 {
                return Ok(Some((source.0 - 1, source.1)));
            }
        }

        return Ok(None);
    }

    /// Make a move, returning a new position.
    pub fn make_move(&self, mov: UciMove) -> ChessResult<Position> {
        use Piece::*;

        // Check that source square contains a piece from the side to move, and that it belongs to
        // the side to move.
        let source_piece = self.at(mov.source)?;

        if source_piece == EmptySquare {
            return Err(ChessError::NoPiece(mov.source.0, mov.source.1));
        }

        if source_piece.color() != Some(self.side_to_move) {
            return Err(ChessError::WrongSide);
        }

        // Check move is otherwise legal for the piece.
        if !self.is_move_legal(&mov)? {
            return Err(ChessError::IllegalMove);
        }

        // Construct new position with piece moved.
        let mut new_pos = *self;

        new_pos.set_at(mov.dest, source_piece)?;
        new_pos.set_at(mov.source, EmptySquare)?;

        new_pos.side_to_move = if self.side_to_move == Color::White { Color::Black } else { Color::White };
        new_pos.en_passant_square = self.en_passant_square_for_move(&mov)?;

        // If the source pieces is a pawn, and is moving to the en-passant square, we need to
        // remove the pawn it's taking.
        if Some(mov.dest) == self.en_passant_square {
            if source_piece == WhitePawn {
                let capture_square = (mov.dest.0-1, mov.dest.1);
                new_pos.set_at(capture_square, EmptySquare)?;
            }
            else if source_piece == BlackPawn {
                let capture_square = (mov.dest.0+1, mov.dest.1);
                new_pos.set_at(capture_square, EmptySquare)?;
            }
        }

        // TODO: update castling state.

        Ok(new_pos)
    }
}

impl Default for Position {
    fn default() -> Self {
        Self::from_visual(
            "
            rnbqkbnr
            pppppppp
            ........
            ........
            ........
            ........
            PPPPPPPP
            RNBQKBNR",
            Color::White,
            None,
            true,
            true,
            true,
            true)
            .expect("Invalid default position")
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
                write!(f, "{}", piece.to_char())?;
            }
            writeln!(f, "")?;
        }

        // If there's an en-passant square, print that too.
        if let Some(en_passant_square) = self.en_passant_square {
            writeln!(f, "\nEn-passant square: ({}, {})", en_passant_square.0, en_passant_square.1)?;
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
        assert_eq!(pos.en_passant_square, None);
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

    // Test making a move.
    #[test]
    fn test_make_move() -> ChessResult<()> {
        // TODO: check all move legalities etc. It's tricky code so it needs to be tested
        // thoroughly. We need to check all the cases handled in is_move_legal and make_move.
        Ok(())
    }
}
