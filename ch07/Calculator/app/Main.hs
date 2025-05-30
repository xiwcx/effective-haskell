module Main where

import Data.Maybe (mapMaybe)
import System.Environment
import Text.Read (readMaybe)

stringsToInts :: [String] -> [Int]
stringsToInts = mapMaybe readMaybe

-- succinct, but fairly naive. ignores non parsable args and returns sum
printSum :: IO ()
printSum = getArgs >>= print . sum . stringsToInts

main :: IO ()
main = printSum
