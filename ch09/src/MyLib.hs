{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module MyLib where

import Data.String
import Text.Read (readMaybe)

-- A functor represents a class of things that allow us
-- to apply a function tah would chagne the type or value
-- of the Functor without changing its underlying structure
class Wunctor f where
  wfmap :: (a -> b) -> f a -> f b
  (<$) :: a -> f b -> f a
  (<$) a fb = wfmap (const a) fb

infixl 4 <$>

(<$>) :: (Wunctor f) => (a -> b) -> f a -> f b
(<$>) = wfmap

data Waybe a = Wothing | Wust a

instance Wunctor Waybe where
  wfmap _ Wothing = Wothing
  wfmap f (Wust a) = Wust (f a)

data List a = Empty | Cons a (List a)

toList :: [a] -> List a
-- uses `Empty` as default value, if there are no xs
-- it will not be overwritten
-- toList xs = foldr Cons Empty xs
-- toList = foldr Cons Empty -- point-free
toList [] = Empty
toList (x : xs) = Cons x (toList xs)

fromList :: List a -> [a]
fromList Empty = []
fromList (Cons x xs) = x : fromList xs

instance Wunctor List where
  wfmap _ Empty = Empty
  wfmap f (Cons a as) = Cons (f a) (wfmap f as)

data Weither a b = Weft a | Wight b

instance Wunctor (Weither a) where
  wfmap f (Weft x) = Weft x
  wfmap f (Wight x) = Wight (f x)

newtype ReverseEither a b = ReverseEither (Either b a)
  deriving (Show)

instance Wunctor (ReverseEither a) where
  wfmap f (ReverseEither (Left x)) = ReverseEither (Left (f x))
  wfmap f (ReverseEither (Right x)) = ReverseEither (Right x)

newtype Function a b = Function {runFunction :: a -> b}

-- writing a functor instance for function is nothing
-- more than function composition
instance Wunctor (Function a) where
  wfmap f (Function g) = Function (f . g)

-- An applicative allows us to use `pure` to introduce
-- a plain value in to a context.
--
-- It also allows us to use `<*>` (Apply, Splat) to take two
-- values that each have their own structure and to combine
-- those structures in some way.
--
-- => : type constraints wok the same for type classes
-- as functions
--
-- anything that is an instance of applicative must also
-- provide an instance for Functor
class (Wunctor f) => Wapplicative f where
  wure :: a -> f a
  infixl 4 <*>
  (<*>) :: f (a -> b) -> f a -> f b

instance Wapplicative Waybe where
  wure = Wust
  Wothing <*> _ = Wothing
  Wust f <*> a = f MyLib.<$> a

instance Wapplicative (Weither a) where
  wure a = Wight a
  (Weft err) <*> _ = Weft err
  (Wight f) <*> g = f MyLib.<$> g

instance Wapplicative List where
  wure :: a -> List a
  wure a = Cons a Empty
  (<*>) :: List (a -> b) -> List a -> List b
  Empty <*> _ = Empty
  Cons f fs <*> vals = (f MyLib.<$> vals) `concatList` (fs MyLib.<*> vals)

concatList :: List a -> List a -> List a
concatList Empty as = as
concatList (Cons a as) bs = Cons a (concatList as bs)

instance Wapplicative (Function a) where
  wure a = Function $ const a
  Function f <*> Function g = Function $ \val -> f val (g val)

class (Wapplicative m) => Wonad m where
  infixl 1 >>=
  (>>=) :: m a -> (a -> m b) -> m b

  infix 1 >>
  (>>) :: m a -> m b -> m b
  a >> b = a MyLib.>>= \_ -> b

  weturn :: a -> m a

instance Wonad Waybe where
  weturn a = Wust a
  Wothing >>= _ = Wothing
  Wust a >>= f = f a

half :: Int -> Maybe Int
half num =
  if even num
    then Just (num `div` 2)
    else Nothing

bound :: (Int, Int) -> Int -> Maybe Int
bound (min, max) num =
  if (num >= min) && (num <= max)
    then Just num
    else Nothing

instance Wonad List where
  weturn a = Cons a Empty
  Empty >>= f = Empty
  Cons a as >>= f = (f a) `concatList` (as MyLib.>>= f)

instance (Show a) => Show (List a) where
  show = show . fromList

instance IsString (List Char) where
  fromString = toList

foo = "Hello, Haqskell " :: List Char

type StringL = List Char

replicateL :: Int -> a -> List a
replicateL 0 _ = Empty
replicateL n a =
  let tail = replicateL (pred n) a
   in Cons a tail

wordsL :: StringL -> List StringL
wordsL = toList . map toList . words . fromList

unwordsL :: List StringL -> StringL
unwordsL = toList . unwords . fromList . (fromList MyLib.<$>)

-- a -> a
-- [Int -> Int]
-- [a]

-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b

-- Just (+) <*> Just 1
-- Maybe (Int -> (Int -> Int)) <*> Maybe Int
-- f: Maybe
-- a: Int
-- b: (Int -> Int)

data Three = Three
  { one :: Int,
    two :: Int,
    three :: Int
  }

-- Int -> Int -> Int -> Three
-- Int -> Int -> Three
-- Int -> Three
-- Three <$> a
-- Maybe (Int -> Int -> Three)