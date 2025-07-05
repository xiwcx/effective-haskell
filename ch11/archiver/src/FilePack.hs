{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module FilePack where

import Data.Bits (shift, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Word
import System.Posix.Types (CMode (..), FileMode)
import Text.Printf
import Text.Read (readEither)

data FileContents
  = StringFileContents String
  | TextFileContents Text
  | ByteStringFileContents ByteString
  deriving (Eq, Read, Show)

data FileData a = FileData
  { fileName :: FilePath,
    fileSize :: Word32,
    filePermissions :: FileMode,
    fileData :: a
  }
  deriving (Eq, Read, Show)

newtype FilePack a = FilePack {getPackedFiles :: [a]} deriving (Eq, Read, Show)

class Encode a where
  encode :: a -> ByteString
  encode = BS.drop 4 . encodeWithSize
  encodeWithSize :: a -> ByteString
  encodeWithSize a =
    let s = encode a
        l = fromIntegral $ BS.length s
     in word32ToByteString l <> s
  {-# MINIMAL encode | encodeWithSize #-}

class Decode a where
  decode :: ByteString -> Either String a

instance Encode ByteString where
  encode = id

instance Decode ByteString where
  decode = Right

instance Encode Text where
  encode = encodeUtf8

instance Decode Text where
  decode = Right . decodeUtf8

instance Encode String where
  encode = BC.pack

instance Decode String where
  decode = Right . BC.unpack

instance Encode Word32 where
  encode = word32ToByteString
  encodeWithSize w =
    let (a, b, c, d) = word32ToBytes w
     in BS.pack [4, 0, 0, 0, a, b, c, d]

instance Decode Word32 where
  decode = bytestringToWord32

instance Encode FileMode where
  -- encode (CMode fMode) = encode fMode
  encode (CMode fMode) = encode (fromIntegral fMode :: Word32)

instance Decode FileMode where
  -- decode = fmap CMode . decode
  decode bs = fmap (CMode . fromIntegral) (decode bs :: Either String Word32)

-- instance (Encode a) => Encode (FileData a) where
--   encode (FileData fName fSize fPermissions fData) =
--     encode fName
--       <> encode fSize
--       <> encode fPermissions
--       <> encode fData

instance (Encode a) => Encode (FileData a) where
  encode (FileData fName fSize fPermissions fData) =
    let encodedFileName = encodeWithSize fName
        encodedFileSize = encodeWithSize fSize
        encodedFilePermissions = encodeWithSize fPermissions
        encodedFileData = encodeWithSize fData
        encodedData =
          encodedFileName
            <> encodedFileSize
            <> encodedFilePermissions
            <> encodedFileData
     in encode encodedData

instance (Encode a, Encode b) => Encode (a, b) where
  encode (a, b) = encode $ encodeWithSize a <> encodeWithSize b

instance {-# OVERLAPPABLE #-} (Encode a) => Encode [a] where
  encode = encode . foldMap encodeWithSize

word32ToBytes :: Word32 -> (Word8, Word8, Word8, Word8)
word32ToBytes word =
  let a = fromIntegral $ 255 .&. word
      b = fromIntegral $ 255 .&. shift word (-8)
      c = fromIntegral $ 255 .&. shift word (-16)
      d = fromIntegral $ 255 .&. shift word (-24)
   in (a, b, c, d)

word32FromBytes :: (Word8, Word8, Word8, Word8) -> Word32
word32FromBytes (a, b, c, d) =
  let a' = fromIntegral a
      b' = shift (fromIntegral b) 8
      c' = shift (fromIntegral c) 16
      d' = shift (fromIntegral d) 24
   in a' .|. b' .|. c' .|. d'

word32ToByteString :: Word32 -> ByteString
word32ToByteString word =
  let (a, b, c, d) = word32ToBytes word
   in BS.pack [a, b, c, d]

bytestringToWord32 :: ByteString -> Either String Word32
bytestringToWord32 bytestring =
  case BS.unpack bytestring of
    [a, b, c, d] -> Right $ word32FromBytes (a, b, c, d)
    _otherwise ->
      let l = show $ BS.length bytestring
       in Left ("Expecting 4 bytes but got" <> l)

consWord32 :: Word32 -> ByteString -> ByteString
consWord32 word bytestring =
  let packedWord = word32ToByteString word
   in packedWord <> bytestring

-- packFiles :: FilePack a -> a
-- packFiles =
--   encode . BC.pack . show

-- unpackFiles :: a -> Either String (FilePack a)
-- unpackFiles serializedData = do
--   decodedData <- decode serializedData

--   readEither $ BC.unpack decodedData

-- sampleFilePack :: FilePack a
-- sampleFilePack =
--   FilePack
--     [ FileData "string.txt" 0 0 $ StringFileContents "hello string",
--       FileData "file.text" 0 0 $ TextFileContents "hello text",
--       FileData "binary.text" 0 0 $ ByteStringFileContents "helloy bytestring"
--     ]

-- testPackFile :: BS.ByteString
-- testPackFile = packFiles sampleFilePack

-- testRoundTrip :: FilePack a -> Bool
-- testRoundTrip pack =
--   Right pack == unpackFiles (packFiles pack)

someFunc :: IO ()
someFunc = putStrLn "someFunc"
