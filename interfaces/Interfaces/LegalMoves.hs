module Interfaces.LegalMoves (
    getLegalMoves
) where

import Common.Types
import Engine.BoardState
import qualified Engine.BitBoard as BitBoard
import qualified Engine.Moves (genMoves, isLegal, Move (..))

getLegalMoves :: BoardState -> Square -> [Square]
getLegalMoves board sq = case getPieceAt board sq of
    Nothing         -> []
    Just (color, p) -> map Engine.Moves.to $ filter ((== sq) . Engine.Moves.from) $ filter (`Engine.Moves.isLegal` board) $ Engine.Moves.genMoves board
