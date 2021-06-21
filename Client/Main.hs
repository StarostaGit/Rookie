import Control.Monad
import qualified Data.Map as Map
import Data.Tuple.Select

import Paths
import Draw

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core


main :: IO ()
main = do
    static <- getStaticDir
    startGUI defaultConfig setup

canvasSize = 800
tileSize = fromIntegral canvasSize / 8

type PieceFile = ((Color, Piece), UI Element)

setup :: Window -> UI ()
setup window = do
    return window # set title "Rookie"

    -- initialize board
    canvas <- UI.canvas
        # set UI.height canvasSize
        # set UI.width  canvasSize
        # set style [("border", "solid black 1px"), ("background", "#eee")]

    startGame <- UI.button #+ [string "Start game"]
    delete    <- UI.button #+ [string "Delete"]
    clear     <- UI.button #+ [string "Clear"]

    out  <- UI.span # set text "Coordinates: "
    wrap <- UI.div #. "wrap" 
        # set style [("width","300px"),("height","300px"),("border","solid black 1px")]
        # set (attr "tabindex") "1"
        #+ [element out]

    getBody window #+
        [ column [element canvas]
        , element startGame, element clear, element wrap
        ]
    
    let images = [UI.loadFile "image/png" "static/staunty/wR.png" >>= \url -> UI.img # set UI.src url]
    let fileInfos = [((White, Rook),   UI.loadFile "image/png" "static/staunty/wR.png"),
                     ((Black, Rook),   UI.loadFile "image/png" "static/staunty/bR.png"),
                     ((White, Knight), UI.loadFile "image/png" "static/staunty/wN.png"),
                     ((Black, Knight), UI.loadFile "image/png" "static/staunty/bN.png"),
                     ((White, Bishop), UI.loadFile "image/png" "static/staunty/wB.png"),
                     ((Black, Bishop), UI.loadFile "image/png" "static/staunty/bB.png"),
                     ((White, Queen),  UI.loadFile "image/png" "static/staunty/wQ.png"),
                     ((Black, Queen),  UI.loadFile "image/png" "static/staunty/bQ.png"),
                     ((White, King),   UI.loadFile "image/png" "static/staunty/wK.png"),
                     ((Black, King),   UI.loadFile "image/png" "static/staunty/bK.png"),
                     ((White, Pawn),   UI.loadFile "image/png" "static/staunty/wP.png"),
                     ((Black, Pawn),   UI.loadFile "image/png" "static/staunty/bP.png")]

    images <- foldl (\acc fileInfo -> do
        let info = sel1 fileInfo
        let file = sel2 fileInfo
        url <- file
        unpackedAcc <- acc
        img <- UI.img # set UI.src url
        let list = (info, img) : unpackedAcc
        (return list :: UI [((Color, Piece), Element)])) (return [] :: UI [((Color, Piece), Element)]) fileInfos

    canvas # draw canvasSize images

    on UI.click startGame $ const $ do
        canvas # draw canvasSize images

    on UI.click clear $ const $ do
        canvas # UI.clearCanvas
