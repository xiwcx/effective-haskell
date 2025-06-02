module Main where

import System.Directory (doesFileExist)
import System.Environment

stringToList :: String -> [String]
stringToList a = [a]

replaceNeedle :: String -> String -> [String] -> [String]
replaceNeedle needle replacement =
  map (\word -> if word == needle then replacement else word)

parseFile :: String -> String -> String -> String
parseFile contents needle replacement =
  unwords . replaceNeedle needle replacement . words $ contents

wordReplacer :: IO ()
wordReplacer = do
  args <- getArgs

  let path = head args
  let needle = args !! 1
  let replacement = args !! 2

  pathExists <- doesFileExist path
  -- don't like this, but can't figure out how to get it in to `parseFile`
  contents <- readFile path

  if pathExists
    then putStrLn (parseFile contents needle replacement)
    else print "file does not exist"

main :: IO ()
main = wordReplacer
