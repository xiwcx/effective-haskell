module NaturalClass where

data Peano = Z | S Peano

toPeano :: Int -> Peano
toPeano 0 = Z
toPeano n = S $ toPeano (n - 1)

fromPeano :: Peano -> Int
fromPeano Z = 0
fromPeano (S n) = 1 + fromPeano n

class (Show n, Eq n) => Natural n where
  add :: n -> n -> n
  multiply :: n -> n -> n
  additiveIdentity :: n -- a + n = a (sum is same regardless of addend)
  multiplicativeIdentity :: n -- a * n = a (product is same regardless of factor)

instance Natural Int where
  add = (+)
  multiply = (*)
  additiveIdentity = 0
  multiplicativeIdentity = 1

instance Eq Peano where
  (==) Z Z = True
  (==) (S a) (S b) = a == b
  (==) _ _ = False

instance Show Peano where
  show Z = "Z"
  show (S a) = mconcat ["(S ", show a, ")"]

instance Natural Peano where
  add a Z = a
  add a (S b) = add (S a) b
  multiply Z _ = Z
  multiply (S a) b = add b (multiply a b)
  additiveIdentity = Z
  multiplicativeIdentity = S Z

showIdentities :: IO ()
showIdentities =
  let mul = multiplicativeIdentity :: Peano
      add = additiveIdentity :: Peano
      msg =
        "The additive identity is: "
          <> show add
          <> " and the multiplicative identity is: "
          <> show mul
   in print msg
