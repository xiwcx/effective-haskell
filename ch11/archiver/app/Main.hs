module Main where

import qualified FilePack (someFunc)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  FilePack.someFunc
