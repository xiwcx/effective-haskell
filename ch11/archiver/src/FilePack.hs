{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
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

-- an existential type, like the following allows you to work with a
-- generic type, hidden aside from its constraints or implementation
-- details
data Packable = forall a. (Encode a) => Packable {getPackable :: FileData a}

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

-- EXISTENTIAL

data SomeExistential b = forall a. SomeExistential
  { someValue :: a,
    modifyValue :: a -> a,
    combineValues :: a -> a -> a,
    consumeValue :: a -> b
  }

addAndMultiplyInt :: (Integral a) => a -> SomeExistential Int
addAndMultiplyInt n =
  SomeExistential
    { someValue = n,
      modifyValue = (+ n),
      combineValues = (*),
      consumeValue = fromIntegral
    }

runExistential :: SomeExistential a -> a
runExistential (SomeExistential someV modifyV combineV consumeV) =
  consumeV $ combineV (modifyV someV) someV

reverseAndUnwordsString :: String -> SomeExistential String
reverseAndUnwordsString s =
  SomeExistential
    { someValue = s,
      modifyValue = reverse,
      combineValues = \a b -> unwords [a, b],
      consumeValue = id
    }

modifyExistential :: (a -> b) -> SomeExistential a -> SomeExistential b
modifyExistential f (SomeExistential someV modifyV combineV consumeV) =
  SomeExistential
    { someValue = someV,
      modifyValue = modifyV,
      combineValues = combineV,
      consumeValue = f . consumeV
    }

instance Functor SomeExistential where
  fmap :: (a -> b) -> SomeExistential a -> SomeExistential b
  fmap = modifyExistential

constExistential :: Int -> SomeExistential Int
constExistential n =
  SomeExistential
    { someValue = n,
      modifyValue = const n,
      combineValues = const $ const n,
      consumeValue = const n
    }

data CanBeShown = forall a. (Show a) => CanBeShown a

showWhatCanBeShown :: CanBeShown -> String
showWhatCanBeShown (CanBeShown value) = show value

instance Show CanBeShown where
  show (CanBeShown a) = show a

data ExistentialPackable = forall a. (Encode a) => ExistentialPackable {getExistentialPackable :: FileData a}

instance Encode ExistentialPackable where
  encode (ExistentialPackable p) = encode p

newtype ExFilePack = ExFilePack [ExistentialPackable]

instance Encode ExFilePack where
  encode (ExFilePack p) = encode p

addFileDataToPack :: (Encode a) => FileData a -> ExFilePack -> ExFilePack
addFileDataToPack a (ExFilePack as) = ExFilePack $ ExistentialPackable a : as

infixr 6 .:

(.:) :: (Encode a) => FileData a -> ExFilePack -> ExFilePack
(.:) = addFileDataToPack

emptyFilePack :: ExFilePack
emptyFilePack = ExFilePack []

testEncodeValue :: ByteString
testEncodeValue =
  let a =
        FileData
          { fileName = "a",
            fileSize = 3,
            filePermissions = 0755,
            fileData = "foo" :: String
          }
      b =
        FileData
          { fileName = "b",
            fileSize = 10,
            filePermissions = 0644,
            fileData = ["hello", "world"] :: [Text]
          }
      c =
        FileData
          { fileName = "c",
            fileSize = 8,
            filePermissions = 0644,
            fileData = (0, "zero") :: (Word32, String)
          }
   in encode $ a .: b .: c .: emptyFilePack