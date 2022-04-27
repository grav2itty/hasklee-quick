module Lib where

import Conduit
import Control.Concurrent hiding (yield)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as Lazy
import Data.ByteString.Builder
import Data.ByteString.Char8 (pack)
import Data.Conduit.Network
import System.FSNotify
import System.FilePath

import Hasklee.ExportBin

haskleeLive :: IO String
haskleeLive = do
  waitForChanges
  return $ unlines
        [ ":r"
        , ":main"
        , ":cmd haskleeLive" ]

waitForChanges :: IO ()
waitForChanges = withManager $ \mgr -> do
    lock <- newEmptyMVar
    stop <- watchTree mgr "." check (const $ putMVar lock ())
    takeMVar lock
    stop
  where
    check event =
      takeFileName (eventPath event) == "Main.hs"

sendBinData :: BinIO a => String -> Int -> a -> IO ()
sendBinData ip port a = do
  bs <- binIO a
  let lbs = toLazyByteString bs
      ll = fromIntegral . Lazy.length $ lbs
  sendByteString ip port . Lazy.toStrict $
    toLazyByteString (int64LE ll) <> toLazyByteString (int64LE 0) <> lbs

sendByteString :: String -> Int -> B.ByteString -> IO ()
sendByteString ip port bs =
  runTCPClient (clientSettings port (Data.ByteString.Char8.pack ip)) $
  \appData -> runConduit (yield bs .| appSink appData)
