module Exercises where

-- EXERCISE: Undefined
-- all of these work because of implicit currying in Haskell
addThree :: Int -> Int -> Int
addThree = undefined

-- addThree a = undefined
-- addThree a b = undefined
-- addThree a b c = undefined

-- EXERCISE: Understanding functions by their Type
swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

conc :: [[a]] -> [a]
conc = foldl (++) []

id' :: a -> a
id' a = a

-- EXERCISE: Filling in Type Holes
-- haskell tooling is better than feedback from IDEs generally

mapApply :: [a -> b] -> [a] -> [b]
mapApply toApply =
  concatMap (\input -> map ($ input) toApply)

example :: [Int] -> String
example = mapApply [lookupLetter] . mapApply offsets
  where
    letters :: [Char]
    letters = ['a' .. 'z']

    lookupLetter :: Int -> Char
    lookupLetter n = letters !! n

    offsets :: [Int -> Int]
    offsets = [rot13, swap10, mixupVowels]

    rot13 :: Int -> Int
    rot13 n = (n + 13) `rem` 26

    swap10 :: Int -> Int
    swap10 n
      | n <= 10 = n + 10
      | n <= 20 = n - 10
      | otherwise = n

    mixupVowels :: Int -> Int
    mixupVowels n =
      case n of
        0 -> 8
        4 -> 14
        8 -> 20
        14 -> 0
        20 -> 4
        n' -> n'
