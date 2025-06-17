{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE KindSignatures #-}

module Selector where

import Data.Kind qualified as Kind

-- simplified version of `Alternative` class
class Select (f :: Kind.Type -> Kind.Type) where
  empty :: f a
  pick :: f a -> f a -> f a

instance Select Maybe where
  empty = Nothing
  pick Nothing a = a
  pick a _ = a

instance Select [] where
  empty = []
  pick = (<>)

selectContact :: Maybe String -> Maybe String -> Maybe String -> Maybe String
selectContact email sms phone =
  case email of
    Just email' -> Just email'
    Nothing ->
      case sms of
        Just sms' -> Just sms'
        Nothing -> phone

newtype MyMaybe a = MyMaybe (Maybe a) deriving (Show)

-- version 1
-- =========

-- instance Semigroup (MyMaybe a) where
--   (MyMaybe Nothing) <> b = b
--   a <> _ = a

-- version 2
-- =========

-- instance Semigroup (MyMaybe a) where
--   (MyMaybe a) <> (MyMaybe b) = MyMaybe (pick a b)

-- instance Monoid (MyMaybe a) where
--   mempty = MyMaybe empty

-- accepts two types and wraps the latter in the former
newtype Sel (f :: Kind.Type -> Kind.Type) (a :: Kind.Type) = Sel (f a)

-- version 3
-- =========

instance (Select f) => Semigroup (Sel f a) where
  (Sel a) <> (Sel b) = Sel (pick a b)

instance (Select f) => Monoid (Sel f a) where
  mempty = Sel empty

newtype MyMaybeDerived a = MyMaybeDerived (Maybe a)
  deriving (Show)
  deriving (Semigroup, Monoid) via (Sel Maybe a)
