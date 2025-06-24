{-# LANGUAGE DerivingStrategies #-}

module ExerciseTwo where

data SortedList a = Empty | Cons a (SortedList a)
  deriving stock (Show, Eq)

insertSorted :: (Ord a) => a -> SortedList a -> SortedList a
insertSorted a Empty = Cons a Empty
insertSorted a (Cons b bs)
  | a >= b = Cons b (insertSorted a bs)
  | otherwise = Cons a (Cons b bs)

-- we can't gaurantee the return of a SortedList without
-- enacting a side effect that violates the functor laws
instance Functor SortedList where
  fmap :: (a -> b) -> SortedList a -> SortedList b
  fmap _ Empty = Empty
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

-- fmap f (Cons x xs) = insertSorted (f x) (Cons (fmap f xs) Empty)