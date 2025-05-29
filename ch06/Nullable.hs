{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}

module Exercises where

import Prelude hiding (null)

-- where definitions are exclusive
-- e.g. all monads must be applicative
-- [original here]

-- default signature add-on method
-- use this for uses where there is a common, but not
-- exclusive use case.
-- e.g. logging method that requires show most of the time
-- but not all the time
class Nullable a where
  isNull :: a -> Bool
  default isNull :: (Eq a) => a -> Bool
  isNull a = a == null
  null :: a

-- push constraint method
-- this is the right call for most cases, allows most flexibility
-- (e.g. some cases don't need equality)
class NullablePush a where
  nullPush :: a

isNullPush :: (Eq a, NullablePush a) => a -> Bool
isNullPush a = a == nullPush

-- this is not an extension of `Nullable` it is an addition to it's definition
-- why not include this in the base definition?
-- class (Eq a) => Nullable a
--

-- remove inference as a debugging method
-- isNull (Just (5 :: Int))
-- is better than
-- isNull (Just 5)

-- given that a is nullable, we can define nullable maybe a as the following
instance (Nullable a) => Nullable (Maybe a) where
  isNull Nothing = True
  isNull (Just a) = isNull a

  null = Nothing

-- isNull (Just 5)

-- instance Nullable (a, b) => (Nullable a, Nullable b) where
--   isNull (isNull a, _) = True
--   isNull (_, isNull b) = True
--   isNull (a, b) = False

--   null = (Nothing, Nothing)

instance (Nullable a, Nullable b) => Nullable (a, b) where
  isNull (a, b) = isNull a && isNull b

  null :: (Nullable a, Nullable b) => (a, b)
  -- null = (null, null)
  null = (null :: a, null :: b)

instance Nullable [a] where
  isNull [] = True
  isNull _ = False
  null = []

-- exercise 3
-- instance Nullable Maybe [a] where
newtype JustIsNull a = JustIsNull (Maybe [a])
  deriving (Nullable) via (Maybe [a])

newtype JustIsNotNull a = JustIsNotNull (Maybe [a])
  deriving (Nullable) via [a]
