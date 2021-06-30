module Engine.Search (
    search
) where

import Common.Types
import Engine.BoardState
import Engine.Moves
import Engine.Eval

-- Define which way the player optimizes
best :: Color -> ((Float, Move) -> (Float, Move) -> (Float, Move))
best White = max
best Black = min

-- Searching algorithm returning best move

search :: BoardState -> Maybe Move
search board =
    let side = sideToMove board
        moves = genMoves board
        evaluated = map (\ move -> (evaluate $ makeMove move board, move)) moves
    in
        if null evaluated then
            Nothing 
        else
            Just $ snd $ foldl1 (best side) evaluated
