module Interfaces.Notation (
    parseFEN, FEN, pieceAsFEN,
    encodeBoardState
) where

import Data.String
import Data.Char
import Data.Array
import Data.List
import Data.List.Split
import Data.Bits
import Control.Monad.State
import qualified Data.Map as Map
import Common.Types
import Engine.BitBoard
import Engine.BoardState
import Engine.Moves (getValidMovesForAll)

type FEN = String

-- FEN to BoardState

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
            board <- get
            let player = sideToMove board
                king = pieces board ! player ! King
                threats = foldl (.|.) 0 $ map (getValidMovesForAll board (opposite player)) $ range (Pawn, King)
            put board { kingInCheck = not $ isEmpty $ threats .&. king }
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

-- BoardState to FEN

encodeBoardState :: BoardState -> FEN
encodeBoardState board =
    let side = if sideToMove board == White then "w" else "b"
        castlingRights = encodeCastling $ castling board
        epSquare = if isEmpty $ enPassant board then "-" else encodeEnPassant $ enPassant board
        halfClock = "0"
        fullClock = show $ (ply board + 1) `div` 2
        mergeNumbers row = concatMap (\xs -> if head xs == '1' then show $ length xs else xs) $ group row
        unfoldedPosition = intercalate "/" $ map (\r -> mergeNumbers $
                                                concatMap (\f ->
                                                    maybe "1" ((: []) . pieceAsFEN) $ getPieceAt board (toEnum $ r * 8 + f)) [0 .. 7]) [7, 6 .. 0]
    in
        unwords [unfoldedPosition, side, castlingRights, epSquare, halfClock, fullClock]

encodeEnPassant :: BitBoard -> String
encodeEnPassant sq =
    let (file, rank) = squareToFileRank (head $ toSquares sq)
        fileEncoded = toEnum $ ord 'a' + file :: Char
        rankEncoded = toEnum $ ord '1' + rank :: Char
    in
        [fileEncoded, rankEncoded]

encodeCastling :: Int -> String
encodeCastling rights =
    let hasRights c p = rights .&. castlingRight c p > 0
        wk = if hasRights White King then "K" else ""
        wq = if hasRights White Queen then "Q" else ""
        bk = if hasRights Black King then "k" else ""
        bq = if hasRights Black Queen then "q" else ""
        record = concat [wk, wq, bk, bq]
    in
        if null record then "-" else record

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
