module ApplicativeLaws where

-- Identity
-- pure id <*> v = v

-- Composition
-- pure (.) <*> u <*> v <*> w = u <*> (v <*> w)

-- Homomorphism
-- pure f <*> pure x = pure (f x)

-- Interchange
-- u <*> pure y = pure ($ y) <*> u

-- ========================================================
-- ========================================================
-- ========================================================

-- :t (<*>)
-- Applicative f => f (a -> b) -> f a -> f b

-- :t (.)
-- (.) :: (b -> c) -> (a -> b) -> a -> c

-- :t
-- pure (.) :: Applicative f => f ((b -> c) -> (a -> b) -> a -> c)

-- this means that u and v in our law need to be functions:
-- pure ((b -> c) -> (a -> b) -> a -> c)
--   <*> f (b -> c)
--   <*> f (a -> b)
--   <*> f a
--   == f (b -> c)
--   <*> (f (a -> b) <*> f a)

-- u :: f (b -> c)
-- v :: f (a -> b)
-- w :: f a

-- w' :: f b
-- w' :: v <*> w

-- result :: f c
--