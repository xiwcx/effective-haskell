module StringParser where

import Data.List (splitAt)

data StringParser = StringParser {runStringParser :: String -> (String, String)}

takeCharacters :: Int -> StringParser
takeCharacters numCharacters = StringParser $
  \inputString -> splitAt numCharacters inputString

getNextWord :: StringParser
getNextWord = StringParser $ \str ->
  case break (== ' ') str of
    (nextWord, "") -> (nextWord, "")
    (nextWord, rest) -> (nextWord, tail rest)

combineParsers :: StringParser -> StringParser -> StringParser
combineParsers first second = StringParser $ \str ->
  let (_, firstResult) = runStringParser first str
   in runStringParser second firstResult

getNextWordAfterTenLetters :: StringParser
getNextWordAfterTenLetters =
  combineParsers (takeCharacters 10) getNextWord

tenLettersAfterTheFirstWord :: StringParser
tenLettersAfterTheFirstWord =
  combineParsers getNextWord (takeCharacters 10)

parseString :: StringParser -> String -> String
parseString parser str =
  fst $ runStringParser parser str
