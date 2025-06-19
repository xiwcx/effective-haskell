module Exercises where

import Data.Bifunctor
import Prelude hiding (Either)

-- ==========
-- Exercise 1
-- ==========

data List a = Empty | Cons a (List a)

instance Functor List where
  -- splat handles both cases now!
  fmap f x = pure f <*> x

instance Applicative List where
  pure :: a -> List a
  pure x = Cons x Empty
  (<*>) :: List (a -> b) -> List a -> List b
  -- (<*>) listOfFunctions list = list >>= \x -> listOfFunctions >>= \y -> Cons (y x) Empty
  (<*>) x y = do
    x' <- x
    y' <- y
    Cons (x' y') Empty

instance Monad List where
  (>>=) :: List a -> (a -> List b) -> List b
  (>>=) = undefined

-- ==========
-- Exercise 3
-- ==========

data Either a b = Left' a | Right' b
  deriving (Show)

instance Functor (Either a) where
  fmap :: (a2 -> b) -> Either a1 a2 -> Either a1 b
  fmap f (Left' x) = Left' x
  fmap f (Right' x) = Right' (f x)

instance Bifunctor Either where
  bimap :: (a -> b) -> (c -> d) -> Either a c -> Either b d
  bimap f f' (Left' x) = Left' (f x)
  bimap f f' (Right' x) = Right' (f' x)
  first :: (a -> b) -> Either a c -> Either b c
  first f = bimap f id
  second :: (b -> c) -> Either a b -> Either a c
  second = bimap id
