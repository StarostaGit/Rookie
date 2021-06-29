module Interfaces.Notation (
    parseFEN, FEN, pieceAsFEN
) where

import Data.String
import Data.Char
import Data.Array
import Data.List.Split
import Data.Bits
import Control.Monad.State
import qualified Data.Map as Map
import Common.Types
import Engine.BitBoard
import Engine.BoardState

type FEN = String

parseFEN :: FEN -> BoardState
parseFEN fen =
    let parse :: FEN -> State BoardState ()
        parse fen = do
            let [position, side, castling, ep, plyClock, move] = words fen
                ranks = concat $ reverse $ splitOn "/" position
            foldl (\ acc p -> acc >>= flip parseSquare p) (return 0) ranks
            modify (\ board -> board { castling = parseCastling castling })
            modify (\ board -> board { enPassant = if ep == "-" then 0
                                                   else square $ (ord (head ep) - ord 'a') + 8 * (ord (ep !! 1) - ord '1') })
            modify (\ board -> board { sideToMove = if side == "w" then White else Black })
    in
        execState (parse fen) emptyBoard

parseSquare :: Int -> Char -> State BoardState Int
parseSquare sq val = do
    board <- get
    if not $ isPiece val then
        return $ sq + digitToInt val
    else do
        let (color, piece) = decodePiece val
            pieceArr = pieces board
            pieceBb = pieceArr ! color ! piece
            oldSide = pieceArr ! color
            newSide = oldSide // [(piece, pieceBb .|. square sq)]
            newPieces = pieceArr // [(color, newSide)]
        modify $ setPieces newPieces
        return $ sq + 1

parseCastling :: String -> Int
parseCastling rights = foldl (\ acc (r, bb) -> if r `elem` rights then acc `xor` bb else acc) 0 [
        ('K', castlingRight White King),
        ('Q', castlingRight White Queen),
        ('k', castlingRight Black King),
        ('q', castlingRight Black Queen)
    ]

-- Helpers

pieceMap :: Map.Map Char (Color, Piece)
pieceMap = Map.fromList [
        ('P', (White, Pawn)),
        ('K', (White, King)),
        ('Q', (White, Queen)),
        ('B', (White, Bishop)),
        ('N', (White, Knight)),
        ('R', (White, Rook)),
        ('p', (Black, Pawn)),
        ('k', (Black, King)),
        ('q', (Black, Queen)),
        ('b', (Black, Bishop)),
        ('n', (Black, Knight)),
        ('r', (Black, Rook))
    ]

pieceAsFEN :: (Color, Piece) -> Char
pieceAsFEN (color, piece) =
    let char = case piece of
            Pawn   -> 'p'
            Knight -> 'n'
            Bishop -> 'b'
            Rook   -> 'r'
            Queen  -> 'q'
            King   -> 'k'
    in
        if color == White then toUpper char else char

isPiece :: Char -> Bool
isPiece p = p `elem` Map.keys pieceMap

decodePiece :: Char -> (Color, Piece)
decodePiece p = case Map.lookup p pieceMap of
    Just val -> val
