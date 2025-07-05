{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Exception (IOException, handle)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Foldable (for_)
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.List (isSuffixOf)
import qualified Data.Set as Set (empty, insert, member)
import System.Directory
  ( canonicalizePath,
    doesDirectoryExist,
    doesFileExist,
    listDirectory,
  )

dropSuffix :: String -> String -> String
dropSuffix suffix s
  | suffix `isSuffixOf` s = take (length s - length suffix) s
  | otherwise = s

data FileType
  = FileTypeDirectory
  | FileTypeRegularFile
  | FileTypeOther

classifyFile :: FilePath -> IO FileType
classifyFile fname = do
  isDirectory <- doesDirectoryExist fname
  isFile <- doesFileExist fname
  pure $ case (isDirectory, isFile) of
    (True, False) -> FileTypeDirectory
    (False, True) -> FileTypeRegularFile
    _otherwise -> FileTypeOther

naiveTraversal :: FilePath -> (FilePath -> a) -> IO [a]
naiveTraversal rootPath action = do
  classification <- classifyFile rootPath
  case classification of
    FileTypeOther -> pure []
    FileTypeRegularFile -> pure $ [action rootPath]
    FileTypeDirectory -> do
      contents <- map (fixPath rootPath) <$> listDirectory rootPath
      results <- concat <$> getPaths contents
      pure results
  where
    fixPath parent fname = parent <> "/" <> fname
    getPaths = mapM (`naiveTraversal` action)

traverseDirectory :: FilePath -> (FilePath -> IO ()) -> IO ()
traverseDirectory rootPath action = do
  seenRef <- newIORef Set.empty
  let haveSeenDirectory canonicalPath =
        Set.member canonicalPath <$> readIORef seenRef

      addDirectoryToSeen canonicalPath =
        modifyIORef seenRef $ Set.insert canonicalPath

      traverseSubdirectory subdirPath = do
        contents <- listDirectory subdirPath
        for_ contents $ \file' ->
          handle @FixIOException (\_ -> pure ()) $ do
            let file = subdirPath <> "/" <> file'
            canonicalPath <- canonicalizePath file
            classification <- classifyFile canonicalPath
            case classification of
              FileTypeOther -> pure ()
              FileTypeRegularFile -> action file
              FileTypeDirectory -> do
                alreadyProcessed <- haveSeenDirectory file
                unless alreadyProcessed $ do
                  addDirectoryToSeen file
                  traverseSubdirectory file
  traverseSubdirectory (dropSuffix "/" rootPath)

traverseDirectory' :: FilePath -> (FilePath -> a) -> IO [a]
traverseDirectory' rootPath action = do
  resultsRef <- newIORef []
  traverseDirectory rootPath $ \file -> do
    let result = action file
    modifyIORef resultsRef (result :)
  readIORef resultsRef

-- Exercise 10.1
traverseDirectoryIO :: FilePath -> (FilePath -> IO a) -> IO [a]
traverseDirectoryIO rootPath action = do
  resultsRef <- newIORef []
  traverseDirectory rootPath $ \file -> do
    result <- action file
    modifyIORef resultsRef (result :)
  readIORef resultsRef

countBytes :: FilePath -> IO (FilePath, Integer)
countBytes path = do
  bytes <- fromIntegral . BS.length <$> BS.readFile path
  pure (path, bytes)

longestContents :: FilePath -> IO ByteString
longestContents rootPath = do
  contentsRef <- newIORef BS.empty
  let takeLongetsFile a b =
        if BS.length a >= BS.length b
          then a
          else b

  traverseDirectory rootPath $ \file -> do
    contents <- BS.readFile file
    modifyIORef contentsRef (takeLongetsFile contents)

  readIORef contentsRef

main :: IO ()
main = putStrLn "Hello, Haskell!"
