module Client.Settings where

import System.FilePath

import Common.Types

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core


canvasSize :: Int
canvasSize = 800

tileSize :: Double
tileSize = fromIntegral canvasSize / 8

selectedTileColor :: UI.FillStyle
selectedTileColor = UI.solidColor $ UI.RGBA {UI.red = 14, UI.green = 114, UI.blue = 39, UI.alpha = 0.5}

loadPieceImage :: (Color, Piece) -> UI String
loadPieceImage (color, piece) =
    let colorName = if color == White then "w" else "b"
        pieceName = case piece of
                        Rook   -> "R"
                        Knight -> "N"
                        Bishop -> "B"
                        Queen  -> "Q"
                        King   -> "K"
                        Pawn   -> "P"
    in
        UI.loadFile "image/png" $ "client/static/staunty" </> (colorName ++ pieceName) <.> ".png"

legalMoveIndicatorSize :: Double
legalMoveIndicatorSize = 15
