module BoardState (
    BoardState
) where

import Data.Word
import Data.Array
import Data.Bits
import BitBoard

type Square = (Int, Int)

data Piece = Pawn | Knight | Bishop | Rook | Queen | King
    deriving (Eq, Ord, Enum, Ix, Show)

data Color = White | Black
    deriving (Eq, Ord, Ix, Show)

-- Complete representation of the board

data BoardState = BoardState {
    pieces :: Array Color (Array Piece BitBoard),
    allPieces :: Array Color BitBoard,
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
            eval = 0
        }

startingPositions :: Color -> Piece -> BitBoard
startingPositions color piece = sum $ map (bit . squareToIndex) $ getStartingSquares color piece

getStartingSquares :: Color -> Piece -> [Square]
getStartingSquares color piece = case color of
    White -> case piece of
        Pawn -> [(1, 0), (1, 1), (1, 2), (1, 3), (1, 4), (1, 5), (1, 6), (1, 7)]
        Knight -> [(0, 1), (0, 6)]
        Bishop -> [(0, 2), (0, 5)]
        Rook -> [(0, 0), (0, 7)]
        Queen -> [(0, 3)]
        King -> [(0, 4)]
    Black -> case piece of
        Pawn -> [(6, 0), (6, 1), (6, 2), (6, 3), (6, 4), (6, 5), (6, 6), (6, 7)]
        Knight -> [(7, 1), (7, 6)]
        Bishop -> [(7, 2), (7, 5)]
        Rook -> [(7, 0), (7, 7)]
        Queen -> [(7, 3)]
        King -> [(7, 4)]

squareToIndex :: Square -> Int
squareToIndex (rank, file) = (fromIntegral rank * 8) + fromIntegral file

-- Piece helper functions
containsPiece :: BoardState -> Square -> Color -> Piece -> Bool
containsPiece state square color piece =
    let i = squareToIndex square
        pos = pieces state ! color ! piece
    in
        testBit pos i

getPieceAt :: BoardState -> Square -> Maybe (Color, Piece)
getPieceAt state square =
    let white = map (\ p -> if containsPiece state square White p then Just (White, p) else Nothing) $ range (Pawn, King)
        black = map (\ p -> if containsPiece state square Black p then Just (Black, p) else Nothing) $ range (Pawn, King)
        filtered = filter (/= Nothing) $ white ++ black
    in
        case filtered of
            []  -> Nothing
            h:t -> h
            