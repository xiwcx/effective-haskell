-- {-# HLINT ignore #-}

module M where

-- import Control.Monad.Trans.Reader
import Data.Kind
import Text.ParserCombinators.ReadP (get)

class Wonad (m :: Type -> Type) where
  myPure :: a -> m a
  myBind :: m a -> (a -> m b) -> m b -- >>=

infixl 1 >>==

(>>==) :: (Wonad m) => m a -> (a -> m b) -> m b
(>>==) = myBind

instance Wonad Maybe where
  myPure :: a -> Maybe a
  myPure = Just
  myBind :: Maybe a -> (a -> Maybe b) -> Maybe b
  myBind (Just x) f = f x
  myBind Nothing _ = Nothing

half :: Int -> Maybe Int
half x = if even x then Just (x `div` 2) else Nothing

-- (myPure 20) >>== (\x -> myPure (x + 1)) >>== half

instance Wonad (Either e) where
  myPure :: a -> Either e a
  myPure = Right
  myBind :: Either e a -> (a -> Either e b) -> Either e b
  myBind (Right x) f = f x
  myBind (Left e) _ = Left e

halfE :: Int -> Either String Int
halfE x = if even x then Right (x `div` 2) else Left "not even"

-- part of family identity, reader, writer...
newtype Identity a = Identity {runIdentity :: a} deriving (Show)

instance Wonad Identity where
  myPure :: a -> Identity a
  myPure = Identity
  myBind :: Identity a -> (a -> Identity b) -> Identity b
  myBind (Identity x) f = f x

step1 :: Int -> Either String Int
step1 x = if x > 0 then Right (x + 1) else Left "step1 failed: non-positive"

step2 :: Int -> Either String Int
step2 x = if even x then Right (x `div` 2) else Left "step2 failed: not even"

step3 :: Int -> Either String Int
step3 x = if x < 10 then Right (x * 3) else Left "step3 failed: too large"

nestedSteps :: Int -> Either String Int
nestedSteps x =
  case step1 x of
    Left e0 -> Left e0
    Right r0 -> case step2 r0 of
      Left e1 -> Left e1
      Right r1 -> case step3 r1 of
        Left e2 -> Left e2
        Right r2 -> Right r2

nestedStepsM :: Int -> Either String Int
nestedStepsM x =
  step1 x >>== \r0 -> step2 r0 >>== \r1 -> step3 r1 >>== \r2 -> myPure r2

nestedStepsMWithActualBind :: Int -> Either String Int
nestedStepsMWithActualBind x =
  step1 x >>= \r0 ->
    step2 r0 >>= \r1 ->
      step3 r1 >>= \r2 ->
        myPure r2

nestedStepsMWithDoSyntax :: Int -> Either String Int
nestedStepsMWithDoSyntax x = do
  r0 <- step1 x
  r1 <- step2 r0
  r2 <- step3 r1
  pure r2

newtype Reeder env a = Reeder {runReeder :: env -> a}

instance Wonad (Reeder env) where
  myPure :: a -> Reeder env a
  myPure x = Reeder (const x) -- could be lamda
  myBind :: Reeder env a -> (a -> Reeder env b) -> Reeder env b
  -- myBind (Reeder ra) rf = Reeder $ \env ->
  --   runReeder (rf (ra env)) env
  myBind (Reeder ra) rf = Reeder $ \env ->
    let a = ra env
        Reeder rb = rf a
     in rb env

myAsk :: Reeder env env
myAsk = Reeder id

myReederComputation :: Reeder Int String
myReederComputation =
  myAsk `myBind` \n -> myPure ("the answer is: " ++ show n)

-- myRealReader :: Reader Int String
-- myRealReader = do
--   n <- ask
--   pure ("the answer is: " ++ show n)

-- state monad
newtype S s a = S {runS :: s -> (a, s)}

instance Wonad (S s) where
  myPure :: a -> S s a
  myPure x = S $ \s -> (x, s)
  myBind :: S s a -> (a -> S s b) -> S s b
  myBind (S sa) sf = S $ \s ->
    let (a, s') = sa s
        S sb = sf a
     in sb s'

getS :: S s s
getS = S $ \s -> (s, s)

putS :: s -> S s ()
putS s = S $ \_ -> ((), s)

-- if this was actual state monad
-- then this could be
-- get >>= \n -> put (n + 1)
addOne :: S Int ()
addOne = getS `myBind` \n -> putS (n + 1)

-- myRealS :: State Int ()
-- myRealS = do
--   n <- get
--   put (n + 1)
--   pure ()