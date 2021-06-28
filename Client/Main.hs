import Control.Monad
import qualified Data.Map as Map
import Data.Tuple.Select
import Data.IORef
import Data.Bifunctor

import Paths
import Draw
import Settings

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core


main :: IO ()
main = do
    static <- getStaticDir
    startGUI defaultConfig setup

type PieceFile = ((Color, Piece), UI Element)

setup :: Window -> UI ()
setup window = void $ do
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

    let positionPieceMap :: Map.Map Position (Color, Piece, Int)
        positionPieceMap = Map.fromList $ [((0, 7), (White, Rook,   0)),
                                           ((7, 7), (White, Rook,   1)),
                                           ((0, 0), (Black, Rook,   0)),
                                           ((7, 0), (Black, Rook,   1)),
                                           ((1, 7), (White, Knight, 0)),
                                           ((6, 7), (White, Knight, 1)),
                                           ((1, 0), (Black, Knight, 0)),
                                           ((6, 0), (Black, Knight, 1)),
                                           ((2, 7), (White, Bishop, 0)),
                                           ((5, 7), (White, Bishop, 1)),
                                           ((2, 0), (Black, Bishop, 0)),
                                           ((5, 0), (Black, Bishop, 1)),
                                           ((3, 7), (White, Queen,  0)),
                                           ((3, 0), (Black, Queen,  0)),
                                           ((4, 7), (White, King,   0)),
                                           ((4, 0), (Black, King,   0))] ++
                                          [((i, 6), (White, Pawn,   i)) | i <- [0..7]] ++
                                          [((i, 1), (Black, Pawn,   i)) | i <- [0..7]]

    let piecePositionsMap :: Map.Map (Color, Piece) [Maybe Position]
        piecePositionsMap = Map.fromList $ map (second (map Just))
                                      [((White, Rook), [(0, 7), (7, 7)]),
                                       ((Black, Rook),    [(0, 0), (7, 0)]),
                                       ((White, Knight),  [(1, 7), (6, 7)]),
                                       ((Black, Knight),  [(1, 0), (6, 0)]),
                                       ((White, Bishop),  [(2, 7), (5, 7)]),
                                       ((Black, Bishop),  [(2, 0), (5, 0)]),
                                       ((White, Queen),   [(3, 7)]),
                                       ((Black, Queen),   [(3, 0)]),
                                       ((White, King),    [(4, 7)]),
                                       ((Black, King),    [(4, 0)]),
                                       ((White, Pawn),    [(i, 6) | i <- [0..7]]),
                                       ((Black, Pawn),    [(i, 1) | i <- [0..7]])]

    currentPieceRef      <- liftIO $ newIORef Nothing
    currentTurnRef       <- liftIO $ newIORef White
    positionPieceMapRef  <- liftIO $ newIORef positionPieceMap
    piecePositionsMapRef <- liftIO $ newIORef piecePositionsMap

    canvas # drawGrid

    on UI.click startGame $ const $ do
        canvas # draw images currentPieceRef piecePositionsMapRef

    on UI.click clear $ const $ do
        canvas # UI.clearCanvas

    on UI.mousedown canvas $ \xy -> do
        canvas # UI.clearCanvas
        canvas # handleClick xy currentPieceRef currentTurnRef (positionPieceMapRef, piecePositionsMapRef)
        canvas # draw images currentPieceRef piecePositionsMapRef
