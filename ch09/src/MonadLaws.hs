module MonadLaws where

-- identity (left-hand)
-- return a >>= m = m s

-- identity (right-hand)
-- m >>= return = m

-- Associativity
-- (a >>= b) >>= c = a >>= (\x -> b c >>= c)

-- ========================================================
-- ========================================================
-- ========================================================

-- compared to functor laws:

-- Functor Identity law
-- fmap id f = id f

-- Monad Identity (Left) law
-- (>>=) (return a) f = f a

-- Monad Identity (Right) law
-- (>>=) m return = m

-- The functor identity law explains restrictions around
-- what is changeable when calling `fmap`. The Monad identity
-- laws are similary, selling us what is changeable when
-- we call `>>=`

-- Monad Identity Law (Left) in terms of IO
--
-- if we insert our "filename" in to an IO action then use
-- >>= to pass it to `getContents` the result should equal
-- passing "filename" directly to `getContents`
--
-- "first execute zero changes, then read the file" will equal
-- "just read the file"
--
-- return "filename" >>= getContents = getContents "filenames

-- Monad Identity Law (Right) in terms of IO
--
-- getContents "filename" >>= return = getContents "filename"

-- ========================================================
-- ========================================================
-- ========================================================

data Outlaw a = Outlaw Int a deriving (Eq, Show)

instance Functor Outlaw where
  -- this counter side effect will break both Functor laws
  fmap f (Outlaw cnt val) = Outlaw (cnt + 1) (f val)

billyTheKid = Outlaw 0 "bank robber"

instance Monad Outlaw where
  return summary = Outlaw 0 summary
  (Outlaw cnt a) >>= f =
    let (Outlaw cnt' v) = f a
     in Outlaw (cnt + cnt' + 1) v

stoleAHorse :: String -> Outlaw String
stoleAHorse = return . (<> " and horse robber")

testLeftIdentity =
  (return "robbed a bank" >>= stoleAHorse) == stoleAHorse "robbed a bank"

testRightIdentity =
  (billyTheKid >>= return) == billyTheKid

-- ========================================================
-- ========================================================
-- ========================================================

-- the associativity law states that the grouping of monadic
-- actions should not effect the outcome as `>>=` ensures
-- that the order will be consistent

-- openFile "/tmp/example.txt" ReadMode >>= hGetContents >>= putStrLn

-- is equal to:

-- openFile "tmp/example.txt" ReadMode >>= (\handle -> hGetContents handle >>= putStrLn)l