{-# LANGUAGE DerivingVia #-}

module DerivingNullable where

import Prelude hiding (null)

class Nullable a where
  isNull :: a -> Bool
  null :: a

-- representing optional values that are only considered null
-- if they are truly missing a value
newtype BasicNullable a = BasicNullable (Maybe a)
  deriving stock (Show)

-- making a `BasicNullable` of a into an instance of `Nullable`
-- with our polymorphic type of a
instance Nullable (BasicNullable a) where
  isNull (BasicNullable Nothing) = True
  isNull _ = False
  null = BasicNullable Nothing

-- representing optional values that will only be considered non-null
-- if they contain a non-null value
newtype TransitiveNullable a = TransitiveNullable (Maybe a)
  deriving stock (Show)

instance (Nullable a) => Nullable (TransitiveNullable a) where
  isNull (TransitiveNullable Nothing) = True
  isNull (TransitiveNullable (Just a)) = isNull a
  null = TransitiveNullable Nothing

instance Nullable [a] where
  isNull [] = True
  isNull _ = False
  null = []

-- `
newtype OptionalString = OptionalString {getString :: Maybe String}
  deriving stock (Show)
  deriving (Nullable) via BasicNullable String

newtype OptionalNonEmptyString = OptionalNonEmptyString {getNonEmptyString :: Maybe String}
  deriving stock (Show)
  deriving (Nullable) via TransitiveNullable String

newtype OptionalList a = OptionalList {getList :: Maybe [a]}
  deriving stock (Show)
  deriving (Nullable) via BasicNullable [a]

newtype OptionalNonEmptyList a = OptionalNonEmptyList {getNonEmptyList :: Maybe [a]}
  deriving stock (Show)
  deriving (Nullable) via TransitiveNullable [a]
