-- Echo server program
module Main (main) where

import Control.Monad (unless, forever, void)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Network.Run.TCP

import Interfaces.Play

main :: IO ()
main = runTCPServer Nothing "3000" talk
  where
    talk s = do
        msg <- recv s 1024
        unless (S.null msg) $ do
          sendAll s $ C.pack $ playMove $ C.unpack msg
          talk s
