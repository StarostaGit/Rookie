module Engine.Eval (
    evaluate
) where

import Data.Array
import Data.Bits
import Common.Types
import Engine.BoardState
import Engine.Moves
import Engine.BitBoard

-- Evaluation function

evaluate :: BoardState -> Float
evaluate board = evaluateMaterial board + evaluateDoubledPawns board + evaluateIsolatedPawns board + evaluateMobility board

-- Raw material evaluation

material :: Piece -> Float
material Pawn = 1
material Knight = 3
material Bishop = 3.5
material Rook = 5
material Queen = 9
material King = 200

evaluateMaterial :: BoardState -> Float
evaluateMaterial board =
    let evaluateForPiece :: Float -> Piece -> Float
        evaluateForPiece = (\ acc piece ->
            let black = pieces board ! Black ! piece
                white = pieces board ! White ! piece
            in
                acc + (fromIntegral (popCount white) - fromIntegral (popCount black)) * material piece)
    in
        foldl evaluateForPiece 0 $ range (Pawn, King)

-- Pawn evaluation

doubledPawnsPenalty :: Float
doubledPawnsPenalty = -0.5

doubledPawnsMasks :: [BitBoard]
doubledPawnsMasks = map file [0 .. 7]

evaluateDoubledPawns :: BoardState -> Float
evaluateDoubledPawns board =
    let pieceArr = pieces board
        black = pieceArr ! Black ! Pawn
        white = pieceArr ! White ! Pawn
        countDoubled :: BitBoard -> Int
        countDoubled pawns = foldl (\ acc mask -> acc + max 0 (popCount (pawns .&. mask) - 1)) 0 doubledPawnsMasks
        blackDoubled = fromIntegral $ countDoubled black :: Float
        whiteDoubled = fromIntegral $ countDoubled white :: Float
    in
        (whiteDoubled - blackDoubled) * doubledPawnsPenalty

isolatedPawnsPenalty :: Float
isolatedPawnsPenalty = -0.5

isolatedPawnsMasks :: [(BitBoard, [BitBoard])]
isolatedPawnsMasks = [(file 0, [file 1])]  ++ [(file i, [file $ i - 1, file $ i + 1]) | i <- [1 .. 6]] ++ [(file 7, [file 6])]

evaluateIsolatedPawns :: BoardState -> Float
evaluateIsolatedPawns board =
    let pieceArr = pieces board
        black = pieceArr ! Black ! Pawn
        white = pieceArr ! White ! Pawn
        countIsolated :: BitBoard -> Int
        countIsolated pawns = foldl (\ acc (central, neighbors) ->
                                    if not (isEmpty (central .&. pawns)) && all (isEmpty . (pawns .&.)) neighbors then acc + popCount (central .&. pawns)
                                    else acc) 0 isolatedPawnsMasks
        blackIsolated = fromIntegral $ countIsolated black :: Float
        whiteIsolated = fromIntegral $ countIsolated white :: Float
    in
        (whiteIsolated - blackIsolated) * isolatedPawnsPenalty

-- Piece mobility

mobility :: Float 
mobility = 0.1

evaluateMobility :: BoardState -> Float 
evaluateMobility board =
    let countMoves :: Color -> Int
        countMoves side = foldl (\ acc piece -> acc + popCount (getValidMovesForAll board side piece)) 0 $ range (Pawn, King)
        black = fromIntegral $ countMoves Black :: Float
        white = fromIntegral $ countMoves White :: Float
    in
        (white - black) * mobility
