{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Exception
import Control.Monad
import Data.Foldable
import Data.IORef
import Data.List
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Data.Time.Clock
  ( diffUTCTime,
    getCurrentTime,
    nominalDiffTimeToSeconds,
  )
import System.Directory
import System.Environment
import Text.Printf

data AppMetrics = AppMetrics
  { successCount :: Int,
    failureCount :: Int,
    callDuration :: Map.Map String Int
  }
  deriving (Eq, Show)

newtype Metrics = Metrics {appMetricsStore :: IORef AppMetrics}

newMetrics :: IO Metrics
newMetrics =
  let emptyAppMetrics =
        AppMetrics
          { successCount = 0,
            failureCount = 0,
            callDuration = Map.empty
          }
   in Metrics <$> newIORef emptyAppMetrics

tickSuccess :: Metrics -> IO ()
tickSuccess (Metrics metricsRef) = modifyIORef metricsRef $
  \m -> m {successCount = 1 + successCount m}

tickFailure :: Metrics -> IO ()
tickFailure (Metrics metricsRef) = modifyIORef metricsRef $
  \m -> m {failureCount = 1 + failureCount m}

timeFunction :: Metrics -> String -> IO a -> IO a
timeFunction (Metrics metrics) actionName action = do
  startTime <- getCurrentTime
  result <- action
  endTime <- getCurrentTime

  modifyIORef metrics $ \oldMetrics ->
    let oldDurationValue =
          fromMaybe 0 $ Map.lookup actionName (callDuration oldMetrics)

        runDuration =
          floor . nominalDiffTimeToSeconds $ diffUTCTime endTime startTime

        newDurationValue = oldDurationValue + runDuration
     in oldMetrics
          { callDuration =
              Map.insert actionName newDurationValue $ callDuration oldMetrics
          }

  pure result

timePureFunction :: Metrics -> String -> a -> IO a
timePureFunction (Metrics metrics) actionName action = do
  startTime <- getCurrentTime
  result <- pure $! action
  endTime <- getCurrentTime

  modifyIORef metrics $ \oldMetrics ->
    let oldDurationValue =
          fromMaybe 0 $ Map.lookup actionName (callDuration oldMetrics)

        runDuration =
          floor . nominalDiffTimeToSeconds $ diffUTCTime endTime startTime

        newDurationValue = oldDurationValue + runDuration
     in oldMetrics
          { callDuration =
              Map.insert actionName newDurationValue $ callDuration oldMetrics
          }

  pure result

displayMetrics :: Metrics -> IO ()
displayMetrics (Metrics metricsStore) = do
  AppMetrics {..} <- readIORef metricsStore
  putStrLn $ "successes: " <> show successCount
  putStrLn $ "failures: " <> show failureCount
  for_ (Map.toList callDuration) $ \(functionName, timing) ->
    putStrLn $ printf "Time spent in \"%s\": %d" functionName timing

-- =========================================================================

data FileType
  = FileTypeDirectory
  | FileTypeRegularFile
  | FileTypeOther

traverseDirectory :: Metrics -> FilePath -> (FilePath -> IO ()) -> IO ()
traverseDirectory metrics rootPath action = do
  seenRef <- newIORef Set.empty
  let haveSeenDirectory canonicalPath =
        Set.member canonicalPath <$> readIORef seenRef

      addDirectoryToSeen canonicalPath =
        modifyIORef seenRef $ Set.insert canonicalPath

      handler ex = print ex >> tickFailure metrics

      traverseSubdirectory subdirPath =
        timeFunction metrics "traverseSubdirectory" $ do
          contents <- listDirectory subdirPath
          for_ contents $ \file' ->
            handle @IOException handler $ do
              let file = subdirPath <> "/" <> file'
              canonicalPath <- canonicalizePath file
              classification <- classifyFile canonicalPath

              result <- case classification of
                FileTypeOther -> pure ()
                FileTypeRegularFile -> action file
                FileTypeDirectory -> do
                  alreadyProcessed <- haveSeenDirectory file
                  unless alreadyProcessed $ do
                    addDirectoryToSeen file
                    traverseSubdirectory file
              tickSuccess metrics
              pure result
  traverseSubdirectory (dropSuffix "/" rootPath)
  where
    classifyFile :: FilePath -> IO FileType
    classifyFile fname = do
      isDirectory <- doesDirectoryExist fname
      isFile <- doesFileExist fname
      pure $ case (isDirectory, isFile) of
        (True, False) -> FileTypeDirectory
        (False, True) -> FileTypeRegularFile
        _otherwise -> FileTypeOther

    dropSuffix :: String -> String -> String
    dropSuffix suffix s
      | suffix `isSuffixOf` s = take (length s - length suffix) s
      | otherwise = s

-- =========================================================================

directorySummaryWithMetrics :: FilePath -> IO ()
directorySummaryWithMetrics root = do
  metrics <- newMetrics
  histogramRef <- newIORef (Map.empty :: Map.Map Char Int)
  traverseDirectory metrics root $ \file -> do
    putStrLn $ file <> ":"
    contents <-
      timeFunction metrics "TextIO.readFile" $
        -- this reads files in to memory every time
        TextIO.readFile file

    timeFunction metrics "wordcount" $
      let wordCount = length $ Text.words contents
       in putStrLn $ "    word count: " <> show wordCount

    timeFunction metrics "histogram" $ do
      oldHistogram <- readIORef histogramRef
      let addCharToHistogram histogram letter =
            Map.insertWith (+) letter 1 histogram
          -- this is lazy by default, creating a thunk
          -- of the earlier function that is storing everything in memory
          -- this thunk will hold on to everything it needs for
          -- eventual computation
          --
          -- the prime or `'` suffixed vesion of a function is typically
          -- a strict (e.g. non-lazy) version of the same function. by
          -- convention only.
          newHistogram = Text.foldl' addCharToHistogram oldHistogram contents
      -- writeIORef histogramRef newHistogram
      -- newHistogram `seq` writeIORef histogramRef newHistogram
      writeIORef histogramRef $! newHistogram
  histogram <- readIORef histogramRef
  putStrLn "Histogram Data:"
  for_ (Map.toList histogram) $ \(letter, count) ->
    putStrLn $ printf "    %c: %d" letter count

  displayMetrics metrics

-- =========================================================================

ioMetrics :: IO (IORef AppMetrics)
ioMetrics =
  newIORef
    AppMetrics
      { successCount = 0,
        failureCount = 0,
        callDuration = Map.empty
      }

printMetrics :: IO ()
printMetrics = do
  ioMetrics >>= readIORef >>= print

incrementSuccess :: IO ()
incrementSuccess =
  ioMetrics >>= flip modifyIORef incrementSuccess'
  where
    incrementSuccess' m =
      m {successCount = 1 + successCount m}

successfullyPrintHello :: IO ()
successfullyPrintHello = do
  print "Hello"
  incrementSuccess

printHelloAndMetrics :: IO ()
printHelloAndMetrics = do
  successfullyPrintHello
  printMetrics

-- =========================================================================

main :: IO ()
main = getArgs >>= directorySummaryWithMetrics . head

characterCounter :: FilePath -> IO (Text.Text -> Int)
characterCounter filePath = do
  haystack <- TextIO.readFile filePath
  pure $ \needle ->
    Text.count needle haystack
      + Text.count needle (Text.pack filePath)

someExample :: FilePath -> IO (IORef Int)
someExample path = do
  countRef <- newIORef 0
  let somePath = complicatedPathFinding path
  counter <- characterCounter somePath
  -- weak head normal form is when every member of an expression
  -- has been evaluated, freeing all references in memory to be
  -- garbage collected
  -- writeIORef countRef (counter " ")
  modifyIORef' countRef (const $ counter " ")
  pure countRef
  where
    complicatedPathFinding :: FilePath -> FilePath
    complicatedPathFinding p =
      let path' = p <> "/some/path"
       in if path' `elem` ["/some/path", "/some/other/path"]
            then path'
            else complicatedPathFinding path'
