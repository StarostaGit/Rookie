module Engine.Moves (
    getValidMoves, getValidMovesForAll,
    Move (..), genMoves, makeMove, isLegal,
    toAlgebraicNotation
) where

import Data.Array
import Data.Bits
import Data.Char
import Data.Word
import Data.Maybe
import Control.Monad.State
import Common.Types
import Engine.BitBoard
import Engine.BoardState

-- Move representation

data Move = Move {
    color :: Color,
    piece :: Piece,
    from :: Square,
    to :: Square,
    castling :: Int,
    promotion :: Maybe Piece,
    isCapture :: Bool,
    isCheck :: Bool
} deriving (Eq, Ord, Show)

-- genMoves generates pseudoLegal moves, as the search will never allow for loosing the king anyway
genMoves :: BoardState -> [Move]
genMoves board =
    let side = sideToMove board
        pieceList = toSquares $ allPieces board ! side
    in
        concatMap (\ sq -> genMovesForPiece board side (case getPieceAt board sq of Just (_, p) -> p) sq) pieceList

genMovesForPiece :: BoardState -> Color -> Piece -> Square -> [Move]
genMovesForPiece board side p sq = flip concatMap (toSquares (getValidMoves board side p $ (square . fromEnum) sq)) $
    (\ targetSq -> flip execState [] $ do
        let theirKing = pieces board ! side ! King
            causesCheck move attacker = not $ isEmpty $ getValidMovesForAll result side attacker .&. theirKing where result = makeMove move board
            move = Move {
                color = side,
                piece = p,
                from = sq,
                to = targetSq,
                Engine.Moves.castling = Engine.BoardState.castling board,
                promotion = Nothing,
                isCapture = containsAnyPiece board targetSq,
                isCheck = False
            }

        if p == Pawn && snd (squareToFileRank (to move)) `elem` promotionRanks then do
            put $ map (\ p -> move { promotion = Just p }) $ range (Knight, Queen)
            modify (map (\ m -> m { isCheck = causesCheck m (case promotion m of Just a -> a) }))
        else do
            put [move]
            modify (map (\ m -> m { isCheck = causesCheck m p }))
    )

makeMove :: Move -> BoardState -> BoardState
makeMove move board = flip execState board $ do
    board <- get
    let oldPieces = pieces board
        newMovedBB = (oldPieces ! color move ! piece move) `xor` square (fromEnum $ from move) `xor` square (fromEnum $ to move)
        newMovedArr = (oldPieces ! color move) // [(piece move, newMovedBB)]
        newPieces = oldPieces // [(color move, newMovedArr)]
    put $ setPieces newPieces board

    when (isCapture move) $ do
        let Just (_, captured) = getPieceAt board $ to move
            newCapturedBB = (newPieces ! opposite (color move) ! captured) `xor` square (fromEnum $ to move)
            newCapturedArr = (newPieces ! opposite (color move)) // [(captured, newCapturedBB)]
        put $ setPieces (newPieces // [(opposite (color move), newCapturedArr)]) board

    modify (\ board -> board { sideToMove = opposite $ color move })
    modify (\ board -> board { kingInCheck = isCheck move })

    let diff = fromEnum (to move) - fromEnum (from move)
        dir = if color move == White then 1 else -1
    if piece move == Pawn && abs diff == 16 then
        modify (\ board -> board { enPassant = square $ fromEnum (from move) + 8 * dir })
    else
        modify (\ board -> board { enPassant = 0 })

    when (isJust $ promotion move) $ do
        board <- get
        let promotedTo = case promotion move of Just p -> p
            oldPieces = pieces board
            newPawnsBB = (oldPieces ! color move ! Pawn) `xor` square (fromEnum $ to move)
            newPromotedBB = (oldPieces ! color move ! promotedTo) `xor` square (fromEnum $ to move)
            newPiecesArr = (oldPieces ! color move) // [(piece move, newPawnsBB), (promotedTo, newPromotedBB)]
            newPieces = oldPieces // [(color move, newPiecesArr)]
        put $ setPieces newPieces board

isLegal :: Move -> BoardState -> Bool
isLegal move board =
    let player = sideToMove board
        newBoard = makeMove move board
        attackedSquares = foldl (.|.) 0 $ map (getValidMovesForAll newBoard (opposite player)) $ range (Pawn, King)
        king = pieces newBoard ! player ! King
    in
        isEmpty $ king .&. attackedSquares

toAlgebraicNotation :: Move -> String
toAlgebraicNotation move =
    let pieceEncoding = array (Knight, King) [(Knight, "N"), (Bishop, "B"), (Rook, "R"), (Queen, "Q"), (King, "K")]
        (originFile, originRank) = squareToFileRank $ from move
        (targetFile, targetRank) = squareToFileRank $ to move
    in
        (if piece move == Pawn then (if isCapture move then [toEnum (originFile + ord 'a') :: Char] else "") else pieceEncoding ! piece move) ++
        (if isCapture move then "x" else "") ++
        [   toEnum (targetFile + ord 'a') :: Char
        ,   toEnum (targetRank + ord '1') :: Char] ++
        (case promotion move of Just p  -> "=" ++ pieceEncoding ! p
                                Nothing -> "") ++
        (if isCheck move then "+" else "")

-- Legal moves generation

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
        (clearFileA, 7),
        (full, 8),
        (clearFileH, 9),
        (clearFileH, 1),
        (clearFileH, -7),
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

promotionRanks :: [Int]
promotionRanks = [0, 7]

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
    let leftAttack = shift (complement (file 7) .&. pawns) (-7)
        rightAttack = shift (complement (file 0) .&. pawns) (-9)
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
        theirPieces = allPieces state ! opposite color
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
        