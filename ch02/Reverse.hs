module Reverse where

reverseFoldL :: (Foldable t) => t a -> [a]
-- guessing this is the faster of the two since prepending is more performant
reverseFoldL lst =
  foldl (\acc x -> x : acc) [] lst

reverseFoldR :: (Foldable t) => t a -> [a]
reverseFoldR lst =
  foldr (\x acc -> acc <> [x]) [] lst