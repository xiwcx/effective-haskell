module MyFunctor where

class Wunctor f where
  -- map :: (a -> b) -> List a -> List b
  wmap :: (a -> b) -> f a -> f b

instance Wunctor [] where
  wmap f [] = []
  wmap f (a:as) = f a : wmap f as

instance Wunctor Maybe where
  wmap :: (a -> b) -> Maybe a -> Maybe b
  wmap f Nothing = Nothing
  wmap f (Just a) = Just (f a)

data BinaryTree a = Leaf | Branch (BinaryTree a) a (BinaryTree a)
  deriving (Show)

instance Wunctor BinaryTree where
  wmap :: (a -> b) -> BinaryTree a -> BinaryTree b 
  wmap f Leaf = Leaf
  wmap f (Branch l c r) = Branch (wmap f l) (f c) (wmap f r)

-- covariant functor
instance Wunctor (Either x) where
  wmap :: (a -> b) -> Either x a -> Either x b
  -- analog between this and `Nothing` example for `Maybe`
  wmap f (Left x) = Left x  
  wmap f (Right x) = Right (f x)