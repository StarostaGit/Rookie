module Settings where

import qualified Graphics.UI.Threepenny as UI


canvasSize :: Int
canvasSize = 800

tileSize :: Double
tileSize = fromIntegral canvasSize / 8

selectedTileColor = UI.solidColor $ UI.RGBA {UI.red = 14, UI.green = 114, UI.blue = 39, UI.alpha = 0.5}