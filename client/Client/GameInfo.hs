module Client.GameInfo where

import Data.Bifunctor
import qualified Data.Map as Map
import Data.Array

import Common.Types


type Position = (Int, Int)

type Board = Map.Map Position (Color, Piece, Int)

-- Information about current game

data GameInfo = GameInfo {
    board :: Board,
    piecePositionsMap :: Map.Map (Color, Piece) [Maybe Position],
    currentPiece :: Maybe (Color, Piece, Int),
    turn :: Color,
    castling :: Array Color (Bool, Bool),
    enPassant :: Maybe Position,
    halfMove :: Int,
    fullMove :: Int
}

initBoard :: Board
initBoard = Map.fromList $ [((0, 7), (White, Rook,   0)),
                            ((7, 7), (White, Rook,   1)),
                            ((0, 0), (Black, Rook,   0)),
                            ((7, 0), (Black, Rook,   1)),
                            ((1, 7), (White, Knight, 0)),
                            ((6, 7), (White, Knight, 1)),
                            ((1, 0), (Black, Knight, 0)),
                            ((6, 0), (Black, Knight, 1)),
                            ((2, 7), (White, Bishop, 0)),
                            ((5, 7), (White, Bishop, 1)),
                            ((2, 0), (Black, Bishop, 0)),
                            ((5, 0), (Black, Bishop, 1)),
                            ((3, 7), (White, Queen,  0)),
                            ((3, 0), (Black, Queen,  0)),
                            ((4, 7), (White, King,   0)),
                            ((4, 0), (Black, King,   0))] ++
                           [((i, 6), (White, Pawn,   i)) | i <- [0..7]] ++
                           [((i, 1), (Black, Pawn,   i)) | i <- [0..7]]

initPiecePositionsMap :: Map.Map (Color, Piece) [Maybe Position]
initPiecePositionsMap = Map.fromList $ map (second (map Just))
                              [((White, Rook), [(0, 7), (7, 7)]),
                               ((Black, Rook),    [(0, 0), (7, 0)]),
                               ((White, Knight),  [(1, 7), (6, 7)]),
                               ((Black, Knight),  [(1, 0), (6, 0)]),
                               ((White, Bishop),  [(2, 7), (5, 7)]),
                               ((Black, Bishop),  [(2, 0), (5, 0)]),
                               ((White, Queen),   [(3, 7)]),
                               ((Black, Queen),   [(3, 0)]),
                               ((White, King),    [(4, 7)]),
                               ((Black, King),    [(4, 0)]),
                               ((White, Pawn),    [(i, 6) | i <- [0..7]]),
                               ((Black, Pawn),    [(i, 1) | i <- [0..7]])]


initGameInfo :: GameInfo
initGameInfo = GameInfo {
    board = initBoard,
    piecePositionsMap = initPiecePositionsMap,
    currentPiece = Nothing,
    turn = White,
    castling = listArray (White, Black) [(True, True), (True, True)],
    enPassant = Nothing,
    halfMove = 0,
    fullMove = 0
}
