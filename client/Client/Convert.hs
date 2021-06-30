module Client.Convert (
    gameInfoToFEN, boardStateToGameInfo, squareToPosition
) where

import Data.List

import Common.Types
import Interfaces.Notation (FEN, pieceAsFEN)
import qualified Data.Map as Map
import Client.GameInfo
import qualified Engine.BoardState as BS
import Engine.BitBoard
import Data.Array
import Data.Char
import Data.Bits


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
            Just (x, y) -> [toEnum $ fromEnum 'a' + x] ++ (show (8 - y))
    in
        intercalate " " [currentPositionToFEN gameInfo, activeColor, castlingAvailibility, enPassantTarget,
                         show $ halfMove gameInfo, show $ fullMove gameInfo]

currentPositionToFEN :: GameInfo -> String
currentPositionToFEN gameInfo = intercalate "/" $
    let mergeNumbers :: String -> String
        mergeNumbers row = concatMap (\xs -> if head xs == '0' then show $ length xs else xs) $ group row
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

squareToPosition :: Square -> Position
squareToPosition square =
    (fromEnum square `mod` 8, 7 - (fromEnum square `div` 8))

boardStateToGameInfo :: BS.BoardState -> GameInfo
boardStateToGameInfo boardState =
    let getBitBoard :: Color -> Piece -> BitBoard
        getBitBoard color piece = BS.pieces boardState ! color ! piece
        getPositions :: Color -> Piece -> [Position]
        getPositions color piece = map squareToPosition $ toSquares $ BS.pieces boardState ! color ! piece
        boardList = concatMap (\color -> concatMap
                                  (\piece ->
                                      let positions = getPositions color piece
                                      in
                                          concatMap (\(pos, index) -> [(pos, (color, piece, index))]) $
                                                    positions `zip` [0.. length positions - 1]
                                  ) $ range (Pawn, King)
                              ) $ range (White, Black)
        piecePositionsList = concatMap (\color -> concatMap
                                           (\piece ->
                                               let positions = getPositions color piece
                                               in
                                                   [((color, piece), map Just positions)]
                                           ) $ range (Pawn, King)
                                       ) $ range (White, Black)
        enPassantBitBoard = BS.enPassant boardState
        castlingValue = BS.castling boardState
    in GameInfo {
        board = Map.fromList boardList,
        piecePositionsMap = Map.fromList piecePositionsList,
        currentPiece = Nothing,
        turn = BS.sideToMove boardState,
        castling = listArray (White, Black) [(BS.castlingRight White King  .&. castlingValue /= 0,
                                              BS.castlingRight White Queen .&. castlingValue /= 0),
                                             (BS.castlingRight Black King  .&. castlingValue /= 0,
                                              BS.castlingRight Black Queen .&. castlingValue /= 0)],
        enPassant = if isEmpty enPassantBitBoard then Nothing else Just $ squareToPosition $ head $ toSquares enPassantBitBoard,
        halfMove = 0,
        fullMove = (BS.ply boardState + 1) `div` 2
    }
