module FunctorLaws where

import Data.Char qualified

data Outlaw a = Outlaw Int a deriving (Eq, Show)

instance Functor Outlaw where
  -- this counter side effect will break both Functor laws
  fmap f (Outlaw cnt val) = Outlaw (cnt + 1) (f val)

bang = (<> "!")

upcase = map Data.Char.toUpper

billyTheKid = Outlaw 0 "bank robber"

-- Functor laws

-- 1. Identity
--
-- fmap id = id
--
-- Mapping the identity function shouldn't change the value of the functor
testIdentity =
  fmap id billyTheKid == id billyTheKid

-- 2. Composition
--
-- fmap (f . g) = fmap f . fmap g
--
-- It should not matter whether we fmap a composed function or compose calls to fmap
testComposition =
  fmap (bang . upcase) billyTheKid == (fmap bang . fmap upcase $ billyTheKid)