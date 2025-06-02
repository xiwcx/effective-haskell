module Exercises where

import Control.Monad qualified

-- imaginary type def for `IO`
data RealWorld

newtype MyIO a = MyIO (RealWorld -> (RealWorld, a))

myReturn :: a -> MyIO a
myReturn a = MyIO $ \r -> (r, a)

-- thinking IO types
-- 7.1.1
-- return is rare, `pure` is preferred
-- inherited from Applicative instead of Monad
nestedIO :: IO (IO String)
nestedIO = return (return "foo")

-- 7.1.2
retrieveNestedIO :: IO (IO a) -> IO a
-- retrieveNestedIO = Control.Monad.join
-- retrieveNestedIO action = action >>= id
retrieveNestedIO action = action >>= \b -> b

-- this is the definiton of id
foo :: a -> a
foo a = a

-- 7.1.3
listIO :: a -> [IO a]
listIO a = [return a]

-- maybe for streamed input?
sequenceIO :: [IO a] -> IO [a]
sequenceIO [] = return []
sequenceIO (x : xs) = do
  x' <- x
  xs' <- sequenceIO xs
  return $ x' : xs'