{-# LANGUAGE OverloadedStrings #-}

module FilePack where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64 (decode, encode)
import qualified Data.ByteString.Char8 as BC
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word
import System.Posix.Types (CMode (..), FileMode)
import Text.Read (readEither)

data FileContents
  = StringFileContents String
  | TextFileContents Text
  | ByteStringFileContents ByteString
  deriving (Eq, Read, Show)

data FileData = FileData
  { fileName :: FilePath,
    fileSize :: Word32,
    filePermissions :: FileMode,
    fileData :: FileContents
  }
  deriving (Eq, Read, Show)

newtype FilePack = FilePack {getPackedFiles :: [FileData]} deriving (Eq, Read, Show)

packFiles :: FilePack -> BS.ByteString
packFiles =
  B64.encode . BC.pack . show

unpackFiles :: BS.ByteString -> Either String FilePack
unpackFiles serializedData = do
  decodedData <- B64.decode serializedData

  readEither $ BC.unpack decodedData

sampleFilePack :: FilePack
sampleFilePack =
  FilePack
    [ FileData "string.txt" 0 0 $ StringFileContents "hello string",
      FileData "file.text" 0 0 $ TextFileContents "hello text",
      FileData "binary.text" 0 0 $ ByteStringFileContents "helloy bytestring"
    ]

testPackFile :: BS.ByteString
testPackFile = packFiles sampleFilePack

testRoundTrip :: FilePack -> Bool
testRoundTrip pack =
  Right pack == unpackFiles (packFiles pack)

someFunc :: IO ()
someFunc = putStrLn "someFunc"
