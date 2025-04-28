module Curry where

--
customCurry :: ((a, b) -> t) -> a -> b -> t
customCurry f x y = f (x, y)

customUncurry :: (t1 -> t2 -> t3) -> (t1, t2) -> t3
customUncurry f (x, y) = f x y