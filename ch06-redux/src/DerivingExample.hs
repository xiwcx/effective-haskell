{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module DerivingExample where

-- newtype Name = Name {getName :: String} deriving (Eq, Show, Ord)
newtype Name = Name String deriving (Eq, Show, Ord)

data Customer = Customer
  { name :: Name,
    mail :: String,
    email :: String
  }
  deriving (Eq, Show, Ord)

newtype USD = USD {getMillis :: Integer}
  deriving (Eq, Ord, Show, Enum, Num, Real, Integral)

-- instance Num USD where
--   (USD a) + (USD b) = USD (a + b)
--   (USD a) * (USD b) = USD (a * b)
--   abs (USD a) = USD (signum a)
--   signum (USD a) = USD (signum a)
--   fromInteger = USD
--   negate (USD a) = USD (negate a)

-- instance Real USD where
--   toRational (USD a) = toRational a

-- instance Enum USD where
--   toEnum a = USD (toEnum a)
--   fromEnum (USD a) = fromEnum a

-- instance Integral USD where
--   quotRem (USD a) (USD b) =
--     let (a', b') = quotRem a b
--      in (USD a', USD b')
--   toInteger (USD a) = a