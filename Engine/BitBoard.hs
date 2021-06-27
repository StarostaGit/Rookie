module BitBoard where

import Data.Word
import Data.Bits

type BitBoard = Word64

index :: (Int, Int) -> Int
index (file, rank) = rank * 8 + file

fileOf :: Int -> Int
fileOf sq = sq `mod` 8

rankOf :: Int -> Int
rankOf sq = sq `div` 8

square :: Int -> BitBoard
square = bit

rank :: Int -> BitBoard
rank r = shiftL 255 (r * 8)

file :: Int -> BitBoard
file f = foldl (.|.) 0 [square (8 * i + f) | i <- [0..7]]

diag :: Int -> BitBoard
diag sq =
    let file = sq `mod` 8
        rank = sq `div` 8
        up = foldl (.|.) 0 [square $ index (file + i, rank + i) | i <- [0 .. (7 - max rank file)]]
        down = foldl (.|.) 0 [square $ index (file - i, rank - i) | i <- [0 .. min rank file]]
    in
        up .|. down

antiDiag :: Int-> BitBoard
antiDiag sq =
    let file = sq `mod` 8
        rank = sq `div` 8
        up = foldl (.|.) 0 [square $ index (file - i, rank + i) | i <- [0 .. min file (7 - rank)]]
        down = foldl (.|.) 0 [square $ index (file + i, rank - i) | i <- [0 .. min rank (7 - file)]]
    in
        up .|. down

empty :: BitBoard
empty = zeroBits

full :: BitBoard
full = complement empty

isEmpty :: BitBoard -> Bool 
isEmpty = (==) 0

-- Pretty Printing
pprint :: BitBoard -> String
pprint bb =
    let ppRank r = foldr (\f acc -> (show $ fromEnum $ testBit bb $ r*8 + f) ++ " " ++ acc) "" [0 .. 7] in
        foldl (\acc r -> ppRank r ++ "\n" ++ acc) "" [0 .. 7]

printBitBoard :: BitBoard -> IO ()
printBitBoard = putStr . pprint