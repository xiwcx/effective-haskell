module Main where

import System.Directory (doesFileExist)
import System.Environment

stringToList :: String -> [String]
stringToList a = [a]

replaceNeedle :: String -> String -> [String] -> [String]
replaceNeedle needle replacement =
  map (\word -> if word == needle then replacement else word)

parseFile :: String -> String -> String -> String
parseFile needle replacement =
  unwords . replaceNeedle needle replacement . words

wordReplacer :: IO ()
wordReplacer = do
  args <- getArgs

  let path = head args
  let needle = args !! 1
  let replacement = args !! 2

  pathExists <- doesFileExist path

  if pathExists
    then putStrLn . parseFile needle replacement =<< readFile path
    else print "file does not exist"

main :: IO ()
main = wordReplacer
