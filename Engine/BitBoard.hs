module BitBoard where

import Data.Word
import Data.Bits

type BitBoard = Word64

piece :: Int -> BitBoard
piece = bit

rank :: Int -> BitBoard
rank r = shiftL 255 (r * 8)

file :: Int -> BitBoard
file f = foldl (.|.) 0 $ [piece (8 * i + f) | i <- [0..7]]

empty :: BitBoard -> Bool 
empty = (==) 0