module Draw (draw, handleClick, Color(..), Piece(..)) where

import Control.Monad
import qualified Data.Map as Map

import Paths

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

data Color = White | Black
    deriving (Eq, Ord)

data Piece = Rook | Knight | Bishop | Queen | King | Pawn
    deriving (Eq, Ord)

positionMap :: Map.Map (Color, Piece) [UI.Point]
positionMap = Map.fromList [((White, Rook),   [(0, 7), (7, 7)]),
                            ((Black, Rook),   [(0, 0), (7, 0)]),
                            ((White, Knight), [(1, 7), (6, 7)]),
                            ((Black, Knight), [(1, 0), (6, 0)]),
                            ((White, Bishop), [(2, 7), (5, 7)]),
                            ((Black, Bishop), [(2, 0), (5, 0)]),
                            ((White, Queen),  [(3, 7)]),
                            ((Black, Queen),  [(3, 0)]),
                            ((White, King),   [(4, 7)]),
                            ((Black, King),   [(4, 0)]),
                            ((White, Pawn),   [(i, 6) | i <- [0..7]]),
                            ((Black, Pawn),   [(i, 1) | i <- [0..7]])]

draw :: Int -> [((Color, Piece), Element)] -> UI.Canvas -> UI ()
draw canvasSize images canvas = do
    canvas # UI.clearCanvas

    let tileSize = fromIntegral canvasSize / 8

    forM_ [0..7] (\i -> do
        forM_ [0..7] (\j -> do
            let x = j * tileSize
            let y = i * tileSize
            if even (floor i +  floor j)
                then 
                    canvas # set' UI.fillStyle (UI.htmlColor "#dee3e6")
                else 
                    canvas # set' UI.fillStyle (UI.htmlColor "#8ca2ad")
            canvas # UI.fillRect (x, y) tileSize tileSize)
        )

    forM_ images (\((color, piece), img) -> do
        let maybePositions = Map.lookup (color, piece) positionMap
        case maybePositions of Nothing        -> return ()
                               Just positions -> forM_ positions 
                                                       (\(i, j) -> do
                                                           canvas # UI.drawImage img (i * 100, j * 100))
        )
