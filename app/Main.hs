module Main where

import Lib
import Hasklee

ip :: String
ip = "127.0.0.1"

port :: Int
port = 9734

main :: IO ()
main =  sendBinData ip port scene

scene :: NScene Double ()
scene = do
  let obj = solidObj (Cube 1)
  newObj $ translateZ (-5) $ obj
