{-# LANGUAGE ParallelListComp #-}

module Zip where

import Debug.Trace

zipWithRecurse' :: (a -> b -> c) -> [a] -> [b] -> [c]
-- idiomatically we should never have errors, this
-- function is considered partial as it doesn't handle
-- empty lists. head is generally avoided unless a filled
-- list can be garuanteed
zipWithRecurse' fnc aLst bLst =
  let a = head aLst
      b = head bLst
      aLst' = tail aLst
      bLst' = tail bLst
   in if null aLst || null bLst
        then []
        else fnc a b : zipWithRecurse' fnc aLst' bLst'

zipWithRecurse'' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWithRecurse'' fnc _ [] = []
zipWithRecurse'' fnc [] _ = []
zipWithRecurse'' fnc (a : aLst') (b : bLst') = fnc a b : zipWithRecurse'' fnc aLst' bLst'

zipWithLC :: (a -> b -> c) -> [a] -> [b] -> [c]
-- doesn't feel right using zip to replace zip? is there a better option here?
zipWithLC fnc aLst bLst = [fnc a b | a <- aLst | b <- bLst]

concatMapFoldl :: (c -> d) -> [[c]] -> [d]
-- cleaner recursion than foldr
concatMapFoldl fnc = foldl (\cur next -> cur ++ map fnc next) []

concatMapFoldr :: (Show c, Show d) => (c -> d) -> [[c]] -> [d]
-- example of debugging, logging
concatMapFoldr fnc = foldr (\next cur -> traceShow (next, cur) (map fnc next ++ cur)) []

concatFold :: [[a]] -> [a]
concatFold = foldr (++) []
