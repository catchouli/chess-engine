use chess::Position;

pub mod chess {
    /// An enum for representing the color of each side.
    #[repr(u8)]
    #[derive(Debug, Copy, Clone)]
    pub enum Color {
        White,
        Black,
    }

    /// An enum for representing chess pieces, including color. Color is included in order to
    /// minimize the storage of piece + color into a u8.
    #[repr(u8)]
    #[derive(Debug, Copy, Clone)]
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

    #[derive(Copy, Clone)]
    /// A single chess position, including the pieces as an 8x8 matrix and castling state.
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
            todo!();
            Ok(())
        }
    }
}

fn main() {
    // Initialise logging.
    tracing_subscriber::fmt::init();

    // Construct default position.
    let position = Position::default();

    println!("Current position: {position:#?}");
}
