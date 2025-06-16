module MyLib where

unique :: (a -> a -> Bool) -> [a] -> [a]
unique _ [] = []
unique f (elem : elems) =
  let f' a b = not $ f a b
      elems' = filter (f' elem) elems
   in elem : unique f elems'

-- while morally polymorphic, this implementation
-- is ultimately tightly coupled to handling numbers
sumOfUniques ::
  (a -> a -> a) ->
  (a -> a -> Bool) ->
  a ->
  [a] ->
  a
sumOfUniques add compare zero =
  foldr add zero . unique compare

data Natural a = Natural
  { equal :: a -> a -> Bool,
    add :: a -> a -> a,
    multiply :: a -> a -> a,
    additiveIdentity :: a,
    multiplicativeIdentity :: a,
    displayAsString :: a -> String
  }

intNatural :: Natural Int
intNatural =
  Natural
    { equal = (==),
      add = (+),
      multiply = (*),
      additiveIdentity = 0,
      multiplicativeIdentity = 0,
      displayAsString = show
    }

data Peano = Z | S Peano

toPeano :: Int -> Peano
toPeano 0 = Z
toPeano n = S $ toPeano (n - 1)

fromPeano :: Peano -> Int
fromPeano Z = 0
fromPeano (S n) = 1 + fromPeano n

peanoNatural :: Natural Peano
peanoNatural =
  Natural
    { equal = comparePeano,
      add = addPeano,
      multiply = multiplyPeano,
      additiveIdentity = Z,
      multiplicativeIdentity = S Z,
      displayAsString = show . fromPeano
    }
  where
    comparePeano Z Z = True
    comparePeano (S a) (S b) = comparePeano a b
    comparePeano _ _ = False
    addPeano Z b = b
    addPeano (S a) b = addPeano a (S b)
    multiplyPeano Z _ = Z
    multiplyPeano (S a) b =
      addPeano b (multiplyPeano a b)

uniqueNatural :: Natural a -> [a] -> [a]
uniqueNatural _ [] = []
uniqueNatural n (elem : elems) =
  let compare a b = not $ (equal n) a b
      elems' = filter (compare elem) elems
   in elem : uniqueNatural n elems'

-- our type signature is dramatically shortened
-- grouping related functions together is a common
-- haskell pattern. while you may find this implemented
-- with records like this example, haskell provides
-- a built in tool named `typeclass`
sumOfUniqueNaturals :: Natural a -> [a] -> a
sumOfUniqueNaturals n =
  foldr (add n) (additiveIdentity n) . uniqueNatural n

-- 🤯
-- type applications themselves can be polymorphic
showLeftRight :: (Read a, Read b) => String -> Either a b
showLeftRight s
  | length s > 5 = Left (read s)
  | otherwise = Right (read s)