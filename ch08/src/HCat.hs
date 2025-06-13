{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module HCat where

import qualified Control.Exception as Exception
import qualified Data.ByteString as BS
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Data.Time.Clock as Clock
import qualified Data.Time.Clock.POSIX as PosixClock
import qualified Data.Time.Format as TimeFormat
import qualified System.Directory as Directory
import qualified System.Environment as Env
import System.IO
import qualified System.IO.Error as IOError
import qualified System.Info as SystemInfo
import System.Process (readProcess)
import Text.Printf

data ScreenDimensions = ScreenDimensions
  { screenRows :: Int,
    screenColumns :: Int
  }
  deriving (Show)

data ContinueCancel = Continue | Cancel deriving (Eq, Show)

data FileInfo = FileInfo
  { filePath :: FilePath,
    fileSize :: Int,
    fileMTime :: Clock.UTCTime,
    fileReadable :: Bool,
    fileWriteable :: Bool,
    fileExecutable :: Bool
  }
  deriving (Show)

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
runHCat = do
  targetFilePath <- do
    args <- handleArgs
    eitherToErr args

  contents <- do
    handle <- openFile targetFilePath ReadMode
    TextIO.hGetContents handle

  termSize <- getTerminalSize
  hSetBuffering stdout NoBuffering
  finfo <- fileInfo targetFilePath
  let pages = paginate termSize finfo contents
  showPages pages

eitherToErr :: (Show a) => Either a b -> IO b
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
      let (candidate, nextLines) = Text.splitAt lineLength lineText
          (firstLine, overflow) = softWrap candidate (Text.length candidate - 1)
       in firstLine : wordWrap lineLength (overflow <> nextLines)
  where
    softWrap hardwrappedText textIndex
      | textIndex <= 0 = (hardwrappedText, Text.empty)
      | Text.index hardwrappedText textIndex == ' ' =
          let (wrappedLine, rest) = Text.splitAt textIndex hardwrappedText
           in (wrappedLine, Text.tail rest)
      | otherwise = softWrap hardwrappedText (textIndex - 1)

paginate :: ScreenDimensions -> FileInfo -> Text.Text -> [Text.Text]
paginate (ScreenDimensions rows cols) finfo text =
  let rows' = rows - 1
      wrappedLines = concatMap (wordWrap cols) (Text.lines text)
      pages = map (Text.unlines . padTo rows') $ groupsOf rows' wrappedLines
      pageCount = length pages
      statusLines = map (formatFileInfo finfo cols pageCount) [1 .. pageCount]
   in zipWith (<>) pages statusLines
  where
    padTo :: Int -> [Text.Text] -> [Text.Text]
    padTo lineCount rowsToPad =
      take lineCount $ rowsToPad <> repeat ""

-- handle missing tput with generic util
readWithDefault :: String -> Int -> Int
readWithDefault str def =
  if readInt > 0 then readInt else def
  where
    readInt = read str

getTerminalSize :: IO ScreenDimensions
getTerminalSize =
  case SystemInfo.os of
    "darwin" -> tputScreenDimensions
    "linux" -> tputScreenDimensions
    _other -> pure $ ScreenDimensions defaultLines defaultCols
  where
    defaultLines = 25
    defaultCols = 80
    tputScreenDimensions :: IO ScreenDimensions
    tputScreenDimensions = do
      lines <- readProcess "tput" ["lines"] ""
      cols <- readProcess "tput" ["cols"] ""
      let lines' = read $ init lines
          cols' = readWithDefault (init cols) defaultCols
       in return $ ScreenDimensions lines' cols'

getContinue :: IO ContinueCancel
getContinue =
  hSetBuffering stdin NoBuffering
    >> hSetEcho stdin False
    >> getChar
    >>= \case
      ' ' -> return Continue
      'q' -> return Cancel
      _ -> getContinue

showPages :: [Text.Text] -> IO ()
showPages [] = return ()
showPages (page : pages) =
  clearScreen
    >> TextIO.putStrLn page
    >> getContinue
    >>= \case
      Continue -> showPages pages
      Cancel -> return ()

clearScreen :: IO ()
clearScreen =
  BS.putStr "\^[[1J|^[[1;1H"

fileInfo :: FilePath -> IO FileInfo
fileInfo filePath = do
  perms <- Directory.getPermissions filePath
  mtime <- Directory.getModificationTime filePath
  size <- BS.length <$> BS.readFile filePath
  return
    FileInfo
      { filePath = filePath,
        fileSize = size,
        fileMTime = mtime,
        fileReadable = Directory.readable perms,
        fileWriteable = Directory.writable perms,
        fileExecutable = Directory.executable perms
      }

formatFileInfo :: FileInfo -> Int -> Int -> Int -> Text.Text
formatFileInfo FileInfo {..} maxWidth totalPages currentPage =
  let permissionString =
        [ if fileReadable then 'r' else '-',
          if fileWriteable then 'w' else '-',
          if fileExecutable then 'x' else '_'
        ]
      timestamp =
        TimeFormat.formatTime TimeFormat.defaultTimeLocale "%F %T" fileMTime
      statusLine =
        Text.pack $
          printf
            "%s | permissions: %s | %d bytes | modified: %s | page: %d of %d"
            filePath
            permissionString
            fileSize
            timestamp
            currentPage
            totalPages
   in invertText (truncateStatus statusLine)
  where
    invertText inputStr =
      let reverseVideo = "\^[[7m"
          resetVideo = "\^[[0m"
       in reverseVideo <> inputStr <> resetVideo
    truncateStatus statusLine
      | maxWidth <= 3 = ""
      | Text.length statusLine > maxWidth =
          Text.take (maxWidth - 3) statusLine <> "..."
      | otherwise = statusLine
