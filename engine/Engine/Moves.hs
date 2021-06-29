module Engine.Moves (
    getValidMoves, getValidMovesForAll
) where

import Data.Array
import Data.Bits
import Data.Word
import Common.Types
import Engine.BitBoard
import Engine.BoardState

getValidMoves :: BoardState -> Color -> Piece -> BitBoard -> BitBoard
getValidMoves board color p mask = case p of
    Pawn      -> pawnMovesValid board color mask
    Knight    -> knightMovesValid board color mask
    Bishop    -> bishopMovesValid board color mask
    Rook      -> rookMovesValid board color mask
    Queen     -> queenMovesValid board color mask
    King      -> kingMovesValid board color mask

getValidMovesForAll :: BoardState -> Color -> Piece -> BitBoard
getValidMovesForAll board color p = getValidMoves board color p full

-- King

kingMoves :: [(BitBoard, Int)]
kingMoves =
    let clearFileA = complement (file 0)
        clearFileH = complement (file 7)
    in  [
        (clearFileH, 7),
        (full, 8),
        (clearFileH, 9),
        (clearFileH, 1),
        (clearFileA, -7),
        (full, -8),
        (clearFileA, -9),
        (clearFileA, -1)
    ]

genKingMoves :: BitBoard -> BitBoard -> BitBoard
genKingMoves kings ourPieces = complement ourPieces .&. foldl (.|.) empty (map (\ (mask, toShift) -> shift (mask .&. kings) toShift) kingMoves)

kingMovesValid :: BoardState -> Color -> BitBoard -> BitBoard
kingMovesValid state color mask =
    let kings = (.&.) mask (pieces state ! color ! King)
        ourPieces = allPieces state ! color
    in
        genKingMoves kings ourPieces

-- Knight

knightMoves :: [(BitBoard, Int)]
knightMoves =
    let clearFileA = complement (file 0)
        clearFileB = complement (file 1)
        clearFileG = complement (file 6)
        clearFileH = complement (file 7)
    in  [
        (clearFileA .&. clearFileB, 6),
        (clearFileA, 15),
        (clearFileH, 17),
        (clearFileH .&. clearFileG, 10),
        (clearFileH .&. clearFileG, -6),
        (clearFileH, -15),
        (clearFileA, -17),
        (clearFileA .&. clearFileB, -10)
    ]

genKnightMoves :: BitBoard -> BitBoard -> BitBoard
genKnightMoves knights ourPieces = complement ourPieces .&. foldl (.|.) empty (map (\ (mask, toShift) -> shift (mask .&. knights) toShift) knightMoves)

knightMovesValid :: BoardState -> Color -> BitBoard -> BitBoard
knightMovesValid state color mask =
    let knights = (.&.) mask (pieces state ! color ! Knight)
        ourPieces = allPieces state ! color
    in
        genKnightMoves knights ourPieces

-- Pawn

pawnOneStep :: BitBoard -> Color -> BitBoard
pawnOneStep pawns White = shift pawns 8
pawnOneStep pawns Black = shift pawns (-8)

pawnTwoSteps :: BitBoard -> Color -> BitBoard
pawnTwoSteps pawns White = shift (pawns .&. rank 2) 8
pawnTwoSteps pawns Black = shift (pawns .&. rank 5) (-8)

pawnAttacks :: BitBoard -> Color -> BitBoard
pawnAttacks pawns White =
    let leftAttack = shift (complement (file 0) .&. pawns) 7
        rightAttack = shift (complement (file 7) .&. pawns) 9
    in
        leftAttack .|. rightAttack

pawnAttacks pawns Black =
    let leftAttack = shift (complement (file 0) .&. pawns) (-7)
        rightAttack = shift (complement (file 7) .&. pawns) (-9)
    in
        leftAttack .|. rightAttack

genPawnMoves :: Color -> BitBoard -> BitBoard -> BitBoard -> BitBoard
genPawnMoves color pawns ourPieces theirPieces =
    let all = (ourPieces .|. theirPieces)
        oneStep = complement all .&. pawnOneStep pawns color
        twoSteps = complement all .&. pawnTwoSteps oneStep color
        attacks = theirPieces .&. pawnAttacks pawns color
    in
        (oneStep .|. twoSteps) .|. attacks

pawnMovesValid :: BoardState -> Color -> BitBoard -> BitBoard
pawnMovesValid state color mask =
    let pawns = (.&.) mask (pieces state ! color ! Pawn)
        ourPieces = allPieces state ! color
        theirPieces = allPieces state ! opposite color
        epSquare = enPassant state
    in
        genPawnMoves color pawns ourPieces (theirPieces .|. epSquare)

-- Bishop

genDiagMoveset :: Int -> BitBoard -> BitBoard
genDiagMoveset square blockers = genSlidingMoveset square blockers $ diag square

genAntiDiagMoveset :: Int -> BitBoard -> BitBoard
genAntiDiagMoveset square blockers = genSlidingMoveset square blockers $ antiDiag square

genSingleBishopMoveset :: Int -> BitBoard -> BitBoard
genSingleBishopMoveset square blockers = genDiagMoveset square blockers .|. genAntiDiagMoveset square blockers

genBishopMoves :: BitBoard -> BitBoard -> BitBoard -> BitBoard
genBishopMoves bishops ourPieces theirPieces =
    let all = ourPieces .|. theirPieces
        squares = splitPieces bishops
    in
        complement ourPieces .&. foldl (\acc sq -> acc .|. genSingleBishopMoveset sq all) empty squares

bishopMovesValid :: BoardState -> Color -> BitBoard -> BitBoard
bishopMovesValid state color mask =
    let bishops = (.&.) mask (pieces state ! color ! Bishop)
        ourPieces = allPieces state ! color
        theirPieces = allPieces state ! opposite color
    in
        genBishopMoves bishops ourPieces theirPieces

-- Rook

genVerticalMoveset :: Int -> BitBoard -> BitBoard
genVerticalMoveset square blockers = genSlidingMoveset square blockers $ file $ fileOf square

genHorizontalMoveset :: Int -> BitBoard -> BitBoard
genHorizontalMoveset square blockers = genSlidingMoveset square blockers $ rank $ rankOf square

genSingleRookMoveset :: Int -> BitBoard -> BitBoard
genSingleRookMoveset square blockers = genHorizontalMoveset square blockers .|. genVerticalMoveset square blockers

genRookMoves :: BitBoard -> BitBoard -> BitBoard -> BitBoard
genRookMoves rooks ourPieces theirPieces =
    let all = ourPieces .|. theirPieces
        squares = splitPieces rooks
    in
        complement ourPieces .&. foldl (\acc sq -> acc .|. genSingleRookMoveset sq all) empty squares


rookMovesValid :: BoardState -> Color -> BitBoard -> BitBoard
rookMovesValid state color mask =
    let rooks = (.&.) mask (pieces state ! color ! Rook)
        ourPieces = allPieces state ! color
        theirPieces = allPieces state ! opposite color
    in
        genRookMoves rooks ourPieces theirPieces

-- Queen

genQueenMoves :: BitBoard -> BitBoard -> BitBoard -> BitBoard
genQueenMoves queens ourPieces theirPieces = genBishopMoves queens ourPieces theirPieces .|. genRookMoves queens ourPieces theirPieces

queenMovesValid :: BoardState -> Color -> BitBoard -> BitBoard
queenMovesValid state color mask =
    let queens = (.&.) mask (pieces state ! color ! Queen)
        ourPieces = allPieces state ! color
        theirPieces = allPieces state ! color
    in
        genQueenMoves queens ourPieces theirPieces

-- Helper Functions

genSlidingMoveset :: Int -> BitBoard -> BitBoard -> BitBoard
genSlidingMoveset square blockers mask =
    let piece = Engine.BitBoard.square square
        masked = blockers .&. mask
        left = masked - shift piece 1
        right = bitReverse64 (bitReverse64 masked - shift (bitReverse64 piece) 1 ) .&. mask
    in
        (left `xor` right) .&. mask

splitPieces :: BitBoard -> [Int]
splitPieces 0 = []
splitPieces pieces =
    let piece = countTrailingZeros pieces
        piecesLeft = pieces `xor` square piece
    in
        piece : splitPieces piecesLeft
        