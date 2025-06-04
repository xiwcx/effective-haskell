{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module HCat where

import qualified System.Environment as Env
import qualified Control.Exception as Exception
import qualified System.IO.Error as IOError
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Data.ByteString as BS
import System.Process (readProcess)
import qualified System.Info as SystemInfo
import System.IO

data ScreenDimensions = ScreenDimensions
  { screenRows :: Int
  , screenColumns :: Int
  } deriving Show

data ContinueCancel = Continue | Cancel deriving (Eq, Show)

-- truncate args to first arg
handleArgs :: IO (Either String FilePath)
handleArgs =
  parseArgs <$> Env.getArgs
  where
    parseArgs argumentList =
      case argumentList of
        [fname] -> Right fname
        [] -> Left "Error: No arguments provided!"
        _ -> Left "multiple files not supported"

runHCat :: IO ()
runHCat =
  handleIOError $
    handleArgs
    >>= eitherToErr
    >>= flip openFile ReadMode
    >>= TextIO.hGetContents
    >>= \contents ->
      getTerminalSize >>= \ termSize ->
        let pages = paginate termSize contents
        in showPages pages
  where
    handleIOError :: IO () -> IO ()
    handleIOError ioAction =
      Exception.catch ioAction $
      \e -> putStrLn "I ran into an error: " >> print @IOError e

eitherToErr :: Show a => Either a b -> IO b
eitherToErr (Right a) = return a
eitherToErr (Left e) =
  Exception.throwIO . IOError.userError $ show e

groupsOf :: Int -> [a] -> [[a]]
groupsOf n [] = []
groupsOf n elems =
  let (hd, tl) = splitAt n elems
  in hd : groupsOf n tl

wordWrap :: Int -> Text.Text -> [Text.Text]
wordWrap lineLength lineText
  | Text.length lineText <= lineLength = [lineText]
  | otherwise =
    let
      (candidate, nextLines) = Text.splitAt lineLength lineText
      (firstLine, overflow) = softWrap candidate (Text.length candidate - 1)
    in firstLine : wordWrap lineLength (overflow <> nextLines)
    where
      softWrap hardwrappedText textIndex
        | textIndex <= 0 = (hardwrappedText, Text.empty)
        | Text.index hardwrappedText textIndex == ' ' =
          let (wrappedLine, rest) = Text.splitAt textIndex hardwrappedText
          in (wrappedLine, Text.tail rest)
        | otherwise = softWrap hardwrappedText (textIndex - 1)

paginate :: ScreenDimensions -> Text.Text -> [Text.Text]
paginate (ScreenDimensions rows cols) text =
  let unwrappedLines = Text.lines text
      wrappedLines = concatMap (wordWrap cols) unwrappedLines
      pageLines = groupsOf rows wrappedLines
  in map Text.unlines pageLines

getTerminalSize :: IO ScreenDimensions
getTerminalSize =
  case SystemInfo.os of
    "darwin" -> tputScreenDimensions
    "linux" -> tputScreenDimensions
    _other -> pure $ ScreenDimensions 25 80
  where
    tputScreenDimensions :: IO ScreenDimensions
    tputScreenDimensions =
      readProcess "tput" ["lines"] ""
      >>= \lines ->
        readProcess "tput" ["cols"] ""
        >>= \cols ->
          let lines' = read $ init lines
              cols'  = read $ init cols
          in return $ ScreenDimensions lines' cols'

getContinue :: IO ContinueCancel
getContinue =
  hSetBuffering stdin NoBuffering
  >> hSetEcho stdin False
  >> hGetChar stdin
  >>= \input ->
    case input of
      ' ' -> return Continue
      'q' -> return Cancel
      _   -> getContinue

showPages :: [Text.Text] -> IO ()
showPages [] = return ()
showPages (page:pages) =
  clearScreen
  >> TextIO.putStrLn page
  >> getContinue
  >>= \input ->
    case input of
      Continue -> showPages pages
      Cancel   -> return ()

clearScreen :: IO ()
clearScreen =
  BS.putStr "\^[[1J|^[[1;1H"