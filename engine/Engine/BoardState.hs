module Engine.BoardState where

import Data.Word
import Data.Array
import Data.Bits
import Common.Types
import Engine.BitBoard

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

-- Board helper functions

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

emptyBoard :: BoardState
emptyBoard = BoardState {
        pieces = listArray (White, Black) $ replicate 2 (listArray (Pawn, King) (replicate 6 0)),
        allPieces = listArray (White, Black) $ replicate 2 0,
        enPassant = 0,
        castling = 0,
        ply = 0,
        eval = 0
    }

setPieces :: Array Color (Array Piece BitBoard) -> BoardState -> BoardState
setPieces ps board = board {
        pieces = ps,
        allPieces = listArray (White, Black) $ map (foldl (\ acc (_, bb) -> acc `xor` bb) 0 . assocs . snd) (assocs ps)
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
            