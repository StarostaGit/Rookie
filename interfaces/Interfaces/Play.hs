module Interfaces.Play (
    playMove
) where

import Engine.Search
import Engine.Moves
import Interfaces.Notation

playMove :: FEN -> FEN
playMove fen =
    let game = parseFEN fen
        best = search game
    in
        case best of
            Nothing     -> startingPosition
            Just move   -> encodeBoardState response where response = makeMove move game

startingPosition :: FEN 
startingPosition = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"