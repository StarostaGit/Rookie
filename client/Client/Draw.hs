{-# LANGUAGE LambdaCase #-}
module Client.Draw (draw, drawGrid, handleClick, Color(..), Piece(..), Position) where

import Control.Monad
import qualified Data.Map as Map
import Data.IORef
import Data.Maybe

import Common.Types
import Client.Settings
import Client.GameInfo

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core


drawGrid :: UI.Canvas -> UI ()
drawGrid canvas = do
    forM_ [0..7] (\i -> do
        forM_ [0..7] (\j -> do
            let x = j * tileSize
            let y = i * tileSize
            canvas # set' UI.fillStyle (UI.htmlColor $ if even (floor i + floor j) then "#dee3e6" else "#8ca2ad")
            canvas # UI.fillRect (x, y) tileSize tileSize)
        )


draw :: [((Color, Piece), Element)] -> IORef GameInfo -> UI.Canvas -> UI ()
draw images gameInfoRef canvas = do
    canvas # UI.clearCanvas
    canvas # drawGrid

    gameInfo <- liftIO $ readIORef gameInfoRef

    canvas # set' UI.fillStyle selectedTileColor
    runMaybeOperation $ do
        (color, piece, num) <- currentPiece gameInfo
        positions <- Map.lookup (color, piece) $ piecePositionsMap gameInfo
        (posX, posY) <- positions !! num
        return $ do
            canvas # UI.fillRect (fromIntegral posX * 100, fromIntegral posY * 100)
                                 tileSize tileSize

    forM_ images (\((color, piece), img) -> do
        let maybePositions = Map.lookup (color, piece) $ piecePositionsMap gameInfo
        case maybePositions of
            Nothing ->
                return ()
            Just positions ->
                forM_ positions (\case
                                    Nothing -> return ()
                                    Just (i, j) ->
                                        canvas # UI.drawImage img (fromIntegral i * 100, fromIntegral j * 100))
        )

canPromote :: Piece -> Int -> Color -> Bool
canPromote piece posY color =
    piece == Pawn && ((posY == 0 && color == White)  || (posY == 7 && color == Black))

isEnPassant :: Piece -> Position -> Position -> Bool
isEnPassant piece (oldX, oldY) (newX, newY) =
    (piece == Pawn) && (oldX /= newX)

enPassantDeletePosition :: Position -> Position -> Position
enPassantDeletePosition (oldX, oldY) (newX, newY) =
    (newX, oldY)

movePiece :: Maybe Position -> Position -> IORef GameInfo -> IO ()
movePiece currentPiecePositionMaybe clickedPosition gameInfoRef = do
    gameInfo <- readIORef gameInfoRef

    runMaybeOperation $ do
        currentPiecePosition <- currentPiecePositionMaybe
        (color, piece, num)  <- currentPiece gameInfo
        let (_, clickedY) = clickedPosition
        return $ if canPromote piece clickedY color then do
                let maybePositions = Map.lookup (color, Queen) $ piecePositionsMap gameInfo
                    newNum = maybe 0 length maybePositions
                    newBoard = Map.insert clickedPosition (color, Queen, newNum) $
                               Map.delete currentPiecePosition $ board gameInfo
                    newPiecePositionsMap = Map.adjust (\xs -> xs ++ [Just clickedPosition]) (color, Queen) $
                                            Map.adjust (replaceNth num Nothing) (color, piece) $ piecePositionsMap gameInfo
                writeIORef gameInfoRef $ gameInfo {
                                                    board = newBoard,
                                                    piecePositionsMap = newPiecePositionsMap,
                                                    currentPiece = Nothing,
                                                    turn = opposite color
                                                  }
            else if isEnPassant piece currentPiecePosition clickedPosition then do
                    let deletePos = enPassantDeletePosition currentPiecePosition clickedPosition
                        deletePiece = Map.lookup deletePos $ board gameInfo
                        newBoard = Map.insert clickedPosition (color, piece, num) $
                                   Map.delete currentPiecePosition $
                                   Map.delete deletePos $ board gameInfo
                        newPiecePositionsMap = case deletePiece of
                            Nothing ->
                                Map.adjust (replaceNth num $ Just clickedPosition) (color, piece) $ piecePositionsMap gameInfo
                            Just (deleteColor, deletePiece, deleteNum) ->
                                Map.adjust (replaceNth num $ Just clickedPosition) (color, piece) $
                                Map.adjust (replaceNth deleteNum Nothing) (deleteColor, deletePiece) $
                                           piecePositionsMap gameInfo
                    writeIORef gameInfoRef $ gameInfo {
                                                        board = newBoard,
                                                        piecePositionsMap = newPiecePositionsMap,
                                                        currentPiece = Nothing,
                                                        turn = opposite color
                                                      }
                else do
                    let newBoard = Map.insert clickedPosition (color, piece, num) $
                                   Map.delete currentPiecePosition $ board gameInfo
                        newPiecePositionsMap = Map.adjust (replaceNth num $ Just clickedPosition) (color, piece) $
                                                          piecePositionsMap gameInfo
                    writeIORef gameInfoRef $ gameInfo {
                                                        board = newBoard,
                                                        piecePositionsMap = newPiecePositionsMap,
                                                        currentPiece = Nothing,
                                                        turn = opposite color
                                                      }

takePiece :: Maybe Position -> Position -> (Color, Piece, Int) -> IORef GameInfo -> IO ()
takePiece currentPiecePositionMaybe clickedPosition (clickedColor, clickedPiece, clickedNum) gameInfoRef = do
    gameInfo <- readIORef gameInfoRef

    runMaybeOperation $ do
        currentPiecePosition <- currentPiecePositionMaybe
        (color, piece, num)  <- currentPiece gameInfo
        let (_, clickedY) = clickedPosition
        return $ if canPromote piece clickedY color then do
            let maybePositions = Map.lookup (color, Queen) $ piecePositionsMap gameInfo
                newNum = maybe 0 length maybePositions
                newBoard = Map.insert clickedPosition (color, Queen, newNum) $
                           Map.delete currentPiecePosition $ board gameInfo
                newPiecePositionsMap = Map.adjust (\xs -> xs ++ [Just clickedPosition]) (color, Queen) $
                                       Map.adjust (replaceNth num Nothing) (color, piece) $
                                       Map.adjust (replaceNth clickedNum Nothing) (clickedColor, clickedPiece) $
                                                  piecePositionsMap gameInfo
            writeIORef gameInfoRef $ gameInfo {
                board = newBoard,
                piecePositionsMap = newPiecePositionsMap,
                currentPiece = Nothing,
                turn = opposite color
            }
        else do
            let newBoard = Map.insert clickedPosition (color, piece, num) $
                           Map.delete currentPiecePosition $ board gameInfo
                newPiecePositionsMap = Map.adjust (replaceNth num $ Just clickedPosition) (color, piece) $
                                       Map.adjust (replaceNth clickedNum Nothing) (clickedColor, clickedPiece) $
                                                  piecePositionsMap gameInfo
            writeIORef gameInfoRef $ gameInfo {
                board = newBoard,
                piecePositionsMap = newPiecePositionsMap,
                currentPiece = Nothing,
                turn = opposite color
            }

handleClick :: (Double, Double) -> IORef GameInfo -> UI.Canvas -> UI ()
handleClick (x, y) gameInfoRef canvas = do
    let clickedPosition = (floor (x / 100), floor (y / 100))
    gameInfo <- liftIO $ readIORef gameInfoRef

    let clickedPiece = Map.lookup clickedPosition $ board gameInfo
        currentPiecePosition = do (color, piece, num) <- currentPiece gameInfo
                                  result <- Map.lookup (color, piece) $ piecePositionsMap gameInfo
                                  result !! num

    liftIO $ case clickedPiece of
        Nothing ->
            movePiece currentPiecePosition clickedPosition gameInfoRef
        Just (clickedColor, clickedPiece, clickedNum) | clickedColor /= turn gameInfo ->
            takePiece currentPiecePosition clickedPosition (clickedColor, clickedPiece, clickedNum) gameInfoRef
        Just (color, piece, num) | color == turn gameInfo ->
            writeIORef gameInfoRef $ gameInfo { currentPiece = Just (color, piece, num) }
        _ ->
            return ()

-- Helpers

runMaybeOperation :: Monad m => Maybe (m ()) -> m ()
runMaybeOperation = fromMaybe (return ())

replaceNth :: Int -> a -> [a] -> [a]
replaceNth n newVal xs =
    let (left, _:right) = splitAt n xs
    in  left ++ newVal : right

deleteNth :: Int -> [a] -> [a]
deleteNth n xs =
    let (left, _:right) = splitAt n xs
    in  left ++ right

