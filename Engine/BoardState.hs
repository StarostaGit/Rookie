module BoardState where

import Data.Word
import Data.Array
import Data.Bits
import BitBoard

data Square = A1 | B1 | C1 | D1 | E1 | F1 | G1 | H1
            | A2 | B2 | C2 | D2 | E2 | F2 | G2 | H2
            | A3 | B3 | C3 | D3 | E3 | F3 | G3 | H3
            | A4 | B4 | C4 | D4 | E4 | F4 | G4 | H4
            | A5 | B5 | C5 | D5 | E5 | F5 | G5 | H5
            | A6 | B6 | C6 | D6 | E6 | F6 | G6 | H6
            | A7 | B7 | C7 | D7 | E7 | F7 | G7 | H7
            | A8 | B8 | C8 | D8 | E8 | F8 | G8 | H8
    deriving (Eq, Ord, Enum, Ix, Show)

data Piece = Pawn | Knight | Bishop | Rook | Queen | King
    deriving (Eq, Ord, Enum, Ix, Show)

data Color = White | Black
    deriving (Eq, Ord, Ix, Show)

opposite :: Color -> Color
opposite White = Black
opposite Black = White

-- Castling rights

castlingRight :: Color -> Piece -> Int
castlingRight White King = 1
castlingRight White Queen = 2
castlingRight Black King = 4
castlingRight Black Queen = 8
castlingRight _ _ = 0

-- Complete representation of the board

data BoardState = BoardState {
    pieces :: Array Color (Array Piece BitBoard),
    allPieces :: Array Color BitBoard,
    enPassant :: BitBoard,
    castling :: Int, -- k q K Q
    ply :: Int,
    eval :: Int
}

initBoard :: BoardState
initBoard =
    let generateColor = (\ color -> map (startingPositions color) $ range (Pawn, King))
        pieceList = map generateColor $ range (White, Black)
    in
        BoardState {
            pieces = listArray (White, Black) $ map (listArray (Pawn, King)) pieceList,
            allPieces = listArray (White, Black) $ map sum pieceList,
            enPassant = 0,
            castling = castlingRight White King .|. castlingRight White Queen .|. castlingRight Black King .|. castlingRight Black Queen,
            ply = 0,
            eval = 0
        }

startingPositions :: Color -> Piece -> BitBoard
startingPositions color piece = sum $ map squareToBB $ getStartingSquares color piece

getStartingSquares :: Color -> Piece -> [Square]
getStartingSquares color piece = case color of
    White -> case piece of
        Pawn -> range (A2, H2)
        Knight -> [B1, G1]
        Bishop -> [C1, F1]
        Rook -> [A1, H1]
        Queen -> [D1]
        King -> [E1]
    Black -> case piece of
        Pawn -> range (A7, H7)
        Knight -> [B8, G8]
        Bishop -> [C8, F8]
        Rook -> [A8, H8]
        Queen -> [D8]
        King -> [E8]

-- Square helper functions

squareToFileRank :: Square -> (Int, Int)
squareToFileRank sq = (fileOf $ fromEnum sq, rankOf $ fromEnum sq)

squareToBB :: Square -> BitBoard
squareToBB sq = square $ fromEnum sq

genBitBoardFromSquares :: [Square] -> BitBoard
genBitBoardFromSquares squares = foldl xor 0 $ map (square . fromEnum) squares

-- Piece helper functions

containsPiece :: BoardState -> Square -> Color -> Piece -> Bool
containsPiece state square color piece = testBit (pieces state ! color ! piece) $ fromEnum square

getPieceAt :: BoardState -> Square -> Maybe (Color, Piece)
getPieceAt state square =
    let white = map (\ p -> if containsPiece state square White p then Just (White, p) else Nothing) $ range (Pawn, King)
        black = map (\ p -> if containsPiece state square Black p then Just (Black, p) else Nothing) $ range (Pawn, King)
        filtered = filter (/= Nothing) $ white ++ black
    in
        case filtered of
            []  -> Nothing
            h:t -> h
            