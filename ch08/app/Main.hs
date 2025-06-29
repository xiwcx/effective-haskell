module Main where

import Control.Concurrent
import Control.Monad
import System.Console.Terminal.Size
import System.Posix.Signals
import System.Posix.Signals.Exts (windowChange)

-- import HCat (runHCat)

main :: IO ()
main = do
  let handleResize = do
        Just (Window rows cols) <- size
        putStrLn $ "Terminal resized to: " <> show rows <> " rows, " <> show cols <> " columns"

  _ <- installHandler windowChange (Catch handleResize) Nothing

  putStrLn "Listening..."
  forever $ threadDelay 1000000

-- main = HCat.runHCat