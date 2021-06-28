{-# LANGUAGE LambdaCase #-}
module Draw (draw, drawGrid, handleClick, Color(..), Piece(..), Position) where

import Control.Monad
import qualified Data.Map as Map
import Data.IORef

import Paths
import Settings

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core


type Position = (Int, Int)

data Color = White | Black
    deriving (Eq, Ord)

data Piece = Rook | Knight | Bishop | Queen | King | Pawn
    deriving (Eq, Ord)

type MapRefs = (IORef (Map.Map Position (Color, Piece, Int)), IORef (Map.Map (Color, Piece) [Maybe Position]))

drawGrid :: UI.Canvas -> UI ()
drawGrid canvas = do
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


draw :: [((Color, Piece), Element)] -> IORef (Maybe (Color, Piece, Int)) ->
        IORef (Map.Map (Color, Piece) [Maybe Position]) -> UI.Canvas -> UI ()
draw images pieceRef piecePositionsMapRef canvas = do
    canvas # UI.clearCanvas
    canvas # drawGrid

    piecePositionsMap <- liftIO $ readIORef piecePositionsMapRef

    forM_ images (\((color, piece), img) -> do
        let maybePositions = Map.lookup (color, piece) piecePositionsMap
        case maybePositions of
            Nothing ->
                return ()
            Just positions ->
                forM_ positions (\case
                                    Nothing -> return ()
                                    Just (i, j) ->
                                        canvas # UI.drawImage img (fromIntegral i * 100, fromIntegral j * 100))
        )

    currentPiece <- liftIO $ readIORef pieceRef

    runMaybeOperation $ do
        (color, piece, num) <- currentPiece
        positions <- Map.lookup (color, piece) piecePositionsMap
        (posX, posY) <- positions !! num
        return $ do
            canvas # set' UI.fillStyle (UI.htmlColor "green")
            canvas # UI.fillRect (fromIntegral posX * 100, fromIntegral posY * 100)
                                 indicatorSquareSize indicatorSquareSize

finalizeTurn :: IORef (Maybe (Color, Piece, Int)) -> IORef Color -> IO ()
finalizeTurn currentPieceRef currentTurnRef = do
    writeIORef currentPieceRef Nothing
    nextTurn currentTurnRef

canPromote :: Piece -> Int -> Color -> Bool
canPromote piece posY color =
    piece == Pawn && ((posY == 0 && color == White)  || (posY == 7 && color == Black))

enPassant :: Piece -> Position -> Position -> Bool
enPassant piece (oldX, oldY) (newX, newY) =
    (piece == Pawn) && (oldX /= newX)

enPassantDeletePosition :: Position -> Position -> Position
enPassantDeletePosition (oldX, oldY) (newX, newY) =
    (newX, oldY)

movePiece :: Maybe Position -> Position -> IORef (Maybe (Color, Piece, Int)) -> IORef Color -> MapRefs -> IO ()
movePiece currentPiecePositionMaybe clickedPosition currentPieceRef currentTurnRef
          (positionPieceMapRef, piecePositionsMapRef) = do
    positionPieceMap  <- readIORef positionPieceMapRef
    piecePositionsMap <- readIORef piecePositionsMapRef
    currentPiece      <- readIORef currentPieceRef
    currentTurn       <- readIORef currentTurnRef

    runMaybeOperation $ do
        currentPiecePosition <- currentPiecePositionMaybe
        (color, piece, num)  <- currentPiece
        let (_, clickedY) = clickedPosition
        return $ if canPromote piece clickedY color then do
                let maybePositions = Map.lookup (color, Queen) piecePositionsMap
                let newNum = maybe 0 length maybePositions
                writeIORef positionPieceMapRef  $ Map.insert clickedPosition (color, Queen, newNum)
                                                $ Map.delete currentPiecePosition positionPieceMap
                writeIORef piecePositionsMapRef $ Map.adjust (\xs -> xs ++ [Just clickedPosition]) (color, Queen)
                                                $ Map.adjust (replaceNth num Nothing) (color, piece) piecePositionsMap
                finalizeTurn currentPieceRef currentTurnRef
            else if enPassant piece currentPiecePosition clickedPosition then do
                    let deletePos = enPassantDeletePosition currentPiecePosition clickedPosition
                    let deletePiece = Map.lookup deletePos positionPieceMap
                    writeIORef positionPieceMapRef  $ Map.insert clickedPosition (color, piece, num)
                                                    $ Map.delete currentPiecePosition
                                                    $ Map.delete deletePos positionPieceMap
                    case deletePiece of
                        Nothing ->
                            writeIORef piecePositionsMapRef $ Map.adjust (replaceNth num $ Just clickedPosition)
                                                                         (color, piece) piecePositionsMap
                        Just (deleteColor, deletePiece, deleteNum) ->
                            writeIORef piecePositionsMapRef $ Map.adjust (replaceNth num $ Just clickedPosition)
                                                                         (color, piece)
                                                            $ Map.adjust (replaceNth deleteNum Nothing)
                                                                         (deleteColor, deletePiece) piecePositionsMap
                    finalizeTurn currentPieceRef currentTurnRef
                else do
                    writeIORef positionPieceMapRef  $ Map.insert clickedPosition (color, piece, num)
                                                    $ Map.delete currentPiecePosition positionPieceMap
                    writeIORef piecePositionsMapRef $ Map.adjust (replaceNth num $ Just clickedPosition)
                                                                 (color, piece) piecePositionsMap
                    finalizeTurn currentPieceRef currentTurnRef

takePiece :: Maybe Position -> Position -> (Color, Piece, Int) -> IORef (Maybe (Color, Piece, Int)) ->
             IORef Color -> MapRefs -> IO ()
takePiece currentPiecePositionMaybe clickedPosition (clickedColor, clickedPiece, clickedNum) currentPieceRef
          currentTurnRef (positionPieceMapRef, piecePositionsMapRef) = do
    positionPieceMap  <- readIORef positionPieceMapRef
    piecePositionsMap <- readIORef piecePositionsMapRef
    currentPiece      <- readIORef currentPieceRef
    currentTurn       <- readIORef currentTurnRef

    runMaybeOperation $ do
        currentPiecePosition <- currentPiecePositionMaybe
        (color, piece, num)  <- currentPiece
        let (_, clickedY) = clickedPosition
        return $ if canPromote piece clickedY color then do
                let maybePositions = Map.lookup (color, Queen) piecePositionsMap
                let newNum = maybe 0 length maybePositions
                writeIORef positionPieceMapRef  $ Map.insert clickedPosition (color, Queen, newNum)
                                                $ Map.delete currentPiecePosition positionPieceMap
                writeIORef piecePositionsMapRef $ Map.adjust (\xs -> xs ++ [Just clickedPosition]) (color, Queen)
                                                $ Map.adjust (replaceNth num Nothing) (color, piece)
                                                $ Map.adjust (replaceNth clickedNum Nothing)
                                                             (clickedColor, clickedPiece) piecePositionsMap
                finalizeTurn currentPieceRef currentTurnRef else do
                writeIORef positionPieceMapRef  $ Map.insert clickedPosition (color, piece, num)
                                                $ Map.delete currentPiecePosition positionPieceMap
                writeIORef piecePositionsMapRef $ Map.adjust (replaceNth num $ Just clickedPosition) (color, piece)
                                                $ Map.adjust (replaceNth clickedNum Nothing)
                                                             (clickedColor, clickedPiece) piecePositionsMap
                finalizeTurn currentPieceRef currentTurnRef

handleClick :: (Double, Double) -> IORef (Maybe (Color, Piece, Int)) -> IORef Color -> MapRefs -> UI.Canvas -> UI ()
handleClick (x, y) currentPieceRef currentTurnRef (positionPieceMapRef, piecePositionsMapRef) canvas = do
    let clickedPosition = (floor (x / 100), floor (y / 100))
    positionPieceMap  <- liftIO $ readIORef positionPieceMapRef
    piecePositionsMap <- liftIO $ readIORef piecePositionsMapRef
    currentPiece      <- liftIO $ readIORef currentPieceRef
    currentTurn       <- liftIO $ readIORef currentTurnRef

    let clickedPiece = Map.lookup clickedPosition positionPieceMap
    let currentPiecePosition = do (color, piece, num) <- currentPiece
                                  result <- Map.lookup (color, piece) piecePositionsMap
                                  result !! num

    liftIO $ case clickedPiece of
        Nothing ->
            movePiece currentPiecePosition clickedPosition currentPieceRef currentTurnRef
                      (positionPieceMapRef, piecePositionsMapRef)
        Just (clickedColor, clickedPiece, clickedNum) | clickedColor /= currentTurn ->
            takePiece currentPiecePosition clickedPosition (clickedColor, clickedPiece, clickedNum)
                      currentPieceRef currentTurnRef (positionPieceMapRef, piecePositionsMapRef)
        Just (color, piece, num) | color == currentTurn ->
            writeIORef currentPieceRef $ Just (currentTurn, piece, num)
        _ ->
            return ()

-- Helpers

runMaybeOperation :: Monad m => Maybe (m ()) -> m ()
runMaybeOperation m =
    case m of
        Nothing        -> return ()
        Just operation -> operation

replaceNth :: Int -> a -> [a] -> [a]
replaceNth n newVal xs =
    let (left, _:right) = splitAt n xs
    in  left ++ newVal : right

deleteNth :: Int -> [a] -> [a]
deleteNth n xs =
    let (left, _:right) = splitAt n xs
    in  left ++ right

nextTurn :: IORef Color -> IO ()
nextTurn currentTurnRef = do
    val <- readIORef currentTurnRef
    case val of
        White -> writeIORef currentTurnRef Black
        Black -> writeIORef currentTurnRef White