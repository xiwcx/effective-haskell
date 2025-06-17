{-# LANGUAGE DerivingVia #-}

module MyNumericMonoids where

import Data.Semigroup

newtype MySum = MySum {getMySum :: Int}
  deriving (Eq, Show)
  deriving (Semigroup, Monoid) via (Sum Int)

newtype MyProduct a = MyProduct {getMyProduct :: a}
  deriving (Eq, Show)
  deriving (Semigroup, Monoid) via (Product a)