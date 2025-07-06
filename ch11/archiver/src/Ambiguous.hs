{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Ambiguous where

-- generally classes are preferred over existential types
-- they are more idiomatic. they define constraints and
-- don't force you to hide type information.
class SomeClass a b where
  modifyClassValue :: a -> a
  combineClassValues :: a -> a -> a
  consumeClassValues :: a -> b

instance (Integral a) => SomeClass a Int where
  modifyClassValue a = a + a
  combineClassValues = (*)
  consumeClassValues = fromIntegral

runSomeClass :: forall a b. (SomeClass a b) => a -> b
runSomeClass val =
  -- need to be exlicit with `ScopedTypeVariables` to align types here
  let modified = modifyClassValue @a @b val
      combined = combineClassValues @a @b modified val
   in consumeClassValues combined
