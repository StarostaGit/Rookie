import Control.Monad
import qualified Data.Map as Map
import Data.Tuple.Select
import Data.IORef
import Data.Array

import Common.Types

import Client.Draw
import Client.Settings
import Client.GameInfo

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import qualified Data.ByteString.Char8 as C
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Network.Run.TCP


main :: IO ()
main = do
    runTCPClient "127.0.0.1" "3000" $ \s -> do
        startGUI defaultConfig $ setup s

type PieceFile = ((Color, Piece), UI Element)

setup :: Socket -> Window -> UI ()
setup socket window = void $ do
    return window # set title "Rookie"

    -- initialize board
    canvas <- UI.canvas
        # set UI.height canvasSize
        # set UI.width  canvasSize
        # set style [("border", "solid black 1px"), ("background", "#eee")]

    startGame <- UI.button #+ [string "Start game"]
    delete    <- UI.button #+ [string "Delete"]
    clear     <- UI.button #+ [string "Clear"]

    getBody window #+
        [ column [element canvas]
        , element startGame, element clear
        ]

    let fileInfos = [(\x -> (x, loadPieceImage x)) (color, piece) | color <- range (White, Black),
                                                                    piece <- range (Pawn, King)]

    images <- foldl (\acc fileInfo -> do
        let info = sel1 fileInfo
            file = sel2 fileInfo
        url <- file
        unpackedAcc <- acc
        img <- UI.img # set UI.src url
        let list = (info, img) : unpackedAcc
        (return list :: UI [((Color, Piece), Element)])) (return [] :: UI [((Color, Piece), Element)]) fileInfos

    gameInfoRef <- liftIO $ newIORef initGameInfo

    canvas # drawGrid

    on UI.click startGame $ const $ do
        canvas # draw images gameInfoRef

    on UI.click clear $ const $ do
        canvas # UI.clearCanvas

    on UI.mousedown canvas $ \xy -> do
        canvas # handleClick xy gameInfoRef socket
        canvas # UI.clearCanvas
        canvas # draw images gameInfoRef
