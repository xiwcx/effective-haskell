module Zip where

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' fnc aLst bLst =
  let a = head aLst
      b = head bLst
      aLst' = tail aLst
      bLst' = tail bLst
   in if null aLst || null bLst
        then []
        else fnc a b : zipWith' fnc aLst' bLst'

zipWithLC :: (a -> b -> c) -> [a] -> [b] -> [c]
-- doesn't feel right using zip to replace zip? am i doing something wrong here?
zipWithLC fnc aLst bLst = [fnc a b | (a, b) <- zip aLst bLst]
