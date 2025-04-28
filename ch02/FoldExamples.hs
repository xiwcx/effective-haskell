module FoldExamples where

import Prelude hiding (foldl, foldr)

foldl :: (t -> a -> t) -> t -> [a] -> t
foldl func carryValue lst =
  if null lst
    then carryValue
    else foldl func (func carryValue (head lst)) (tail lst)

foldr :: (a1 -> a2 -> a2) -> a2 -> [a1] -> a2
foldr func carryValue lst =
  if null lst
    then carryValue
    else func (head lst) $ foldr func carryValue (tail lst)

map' :: (a1 -> a2) -> [a1] -> [a2]
map' f xs =
  if null xs
    then []
    else f (head xs) : map' f (tail xs)
