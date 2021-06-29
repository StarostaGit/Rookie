module Interfaces.LegalMoves (
    getLegalMoves
) where

import Common.Types
import Engine.BoardState
import qualified Engine.BitBoard as BitBoard
import qualified Engine.Moves (getValidMoves)

getLegalMoves :: BoardState -> Square -> [Square]
getLegalMoves board sq = case getPieceAt board sq of
    Nothing         -> []
    Just (color, p) -> BitBoard.toSquares $ Engine.Moves.getValidMoves board color p (BitBoard.square $ fromEnum sq)
