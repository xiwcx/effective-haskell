module Monad where

-- inductively defined data type
data List a = Empty | Cons a (List a)
  deriving (Show)

-- go back through and write my own datatypes and instances
-- write own Maybe, List, and Tree (functor, instance only)
instance Functor List where
  fmap f Empty = Empty
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)