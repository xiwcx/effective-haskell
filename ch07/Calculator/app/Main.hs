module Main where

import Data.Maybe (mapMaybe)
import System.Environment
import Text.Read (readMaybe)

stringsToInts :: [String] -> [Int]
stringsToInts = mapMaybe readMaybe

-- succinct, but fairly naive. ignores non parsable args and returns sum
printSum :: IO ()
printSum = getArgs >>= print . sum . stringsToInts

stringToOperand :: String -> Either String (Int -> Int -> Int)
stringToOperand maybeOp =
  case maybeOp of
    "+" -> Right (+)
    "-" -> Right (-)
    "*" -> Right (*)
    "/" -> Right div
    _ -> Left $ "Unsupported operand: " ++ maybeOp

-- apply dynamic operator recursively
getResult :: (Int -> Int -> Int) -> [Int] -> Int
getResult op (first : rest) = foldl op first rest

printResult :: IO ()
printResult = do
  (maybeOperand : maybeInts) <- getArgs

  let op = stringToOperand maybeOperand
  let is = stringsToInts maybeInts

  case op of
    Right o -> print $ show (getResult o is)
    Left msg -> putStrLn msg

main :: IO ()
main = printResult
