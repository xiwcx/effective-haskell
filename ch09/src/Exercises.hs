module Exercises where

import Data.Bifunctor
import Data.Functor.Contravariant
import Prelude hiding (Either)

-- ==========
-- Exercise 1
-- ==========

data List a = Empty | Cons a (List a)

instance Functor List where
  -- splat handles both cases now!
  -- fmap f m = m >>= (return . f)
  fmap f x = pure f <*> x

-- why is this is possible? until Applicative was introduced in 2008
-- this was always done with Monads which were overkill for the job
-- as the sequencing isn't necessary here. in this instance you can
-- use a more powerful tool tool from the same family to do the job
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
  return :: a -> List a
  return = undefined
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
  bimap f _ (Left' x) = Left' (f x)
  bimap _ f (Right' x) = Right' (f x)
  first :: (a -> b) -> Either a c -> Either b c
  first f = bimap f id
  second :: (b -> c) -> Either a b -> Either a c
  second = bimap id

-- ==========
-- Exercise 4
-- ==========

newtype Function a b = Function {runFunction :: a -> b}

-- too many variables, not enough translators
-- can be done with multi-param type classes (language extension)
-- but i don't know about that yet
-- can also be done by flipping variables in type definition:
-- newtype Function b a = Function { runFunction :: a -> b }
instance Contravariant (Function a) where
  contramap :: (b -> c) -> Function a c -> Function a b
  -- contramap f (Function g) = Function (g . f)
  contramap f (Function g) = Function undefined

newtype MyPredicate a = MyPredicate {runMyPredicate :: a -> Bool}

instance Contravariant MyPredicate where
  contramap :: (b -> a) -> MyPredicate a -> MyPredicate b
  contramap f (MyPredicate p) = MyPredicate (p . f)

-- ==========
-- Exercise 5
-- ==========

class Profunctor f where
  dimap :: (c -> a) -> (b -> d) -> f a b -> f c d
  lmap :: (c -> a) -> f a b -> f c b
  lmap f = dimap f id
  rmap :: (b -> d) -> f a b -> f a d
  rmap = dimap id

--
instance Profunctor Function where
  dimap :: (c -> a) -> (b -> d) -> Function a b -> Function c d
  dimap f f' (Function x) = Function (f' . x . f)
  lmap :: (c -> a) -> Function a b -> Function c b
  lmap f (Function x) = Function (x . f)
  rmap :: (b -> d) -> Function a b -> Function a d
  rmap f (Function x) = Function (f . x)

-- instance Profunctor Either where
--   dimap :: (c -> a) -> (b -> d) -> Either a b -> Either c d
