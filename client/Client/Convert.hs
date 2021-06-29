module Client.Convert (gameInfoToFEN) where

import Data.List

import Common.Types
import Interfaces.Notation (FEN, pieceAsFEN)
import qualified Data.Map as Map
import Client.GameInfo
import Data.Array
import Data.Char


gameInfoToFEN :: GameInfo -> FEN
gameInfoToFEN gameInfo =
    let activeColor = case turn gameInfo of
            White -> "w"
            Black -> "b"
        castlingAvailibility =
            let getCastling :: (Bool, Bool) -> String
                getCastling (a, b) =
                    let king  = if a then "k" else ""
                        queen = if b then "q" else ""
                    in
                        king ++ queen
                whiteCastling = getCastling (castling gameInfo ! White)
                blackCastling = getCastling (castling gameInfo ! Black)
            in
                map toUpper whiteCastling ++ blackCastling
        enPassantTarget = case enPassant gameInfo of
            Nothing -> "-"
            Just (x, y) -> [(['a'..] !! x)] ++ (show (8 - y))
    in
        intercalate " " [currentPositionToFEN gameInfo, activeColor, castlingAvailibility, enPassantTarget,
                         show $ halfMove gameInfo, show $ fullMove gameInfo]

currentPositionToFEN :: GameInfo -> String
currentPositionToFEN gameInfo = intercalate "/" $
    let mergeNumbers :: String -> String
        mergeNumbers row = concat $ map (\xs -> if head xs == '0' then show $ length xs else xs) $ group row
    in
        map (\y -> mergeNumbers $ concatMap
                (\x ->
                    let maybePiece = Map.lookup (x, y) $ board gameInfo
                    in
                        case maybePiece of
                            Nothing -> "0"
                            Just (color, piece, _) -> [pieceAsFEN (color, piece)]
                ) [0..7]
            ) [0..7]