module Engine.Search (
    search
) where

import Common.Types
import Engine.BoardState
import Engine.Moves
import Engine.Eval

-- Define which way the player optimizes
best :: Ord a => Color -> a -> a -> a
best White = max
best Black = min

horizon :: Color -> Float
horizon White = fromIntegral $ fst $ floatRange 0.0
horizon Black = fromIntegral $ snd $ floatRange 0.0

-- Searching algorithm returning best move

maxDepth :: Int
maxDepth = 2

search :: BoardState -> Maybe Move
search board =
    let side = sideToMove board
        moves = genMoves board
        evaluated = map (\ move -> (minmax (makeMove move board) side maxDepth, move)) moves
    in
        if null evaluated then
            Nothing 
        else
            Just $ snd $ foldl1 (best side) evaluated

minmax :: BoardState -> Color -> Int -> Float
minmax board player 0 = evaluate board
minmax board player depth = foldl (best player) (horizon player) moves
    where moves = map (\ move -> minmax (makeMove move board) (opposite player) (depth - 1)) $ genMoves board
