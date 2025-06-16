{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- forall in this instance is an example of universal quantification
-- more commonly referred to as "explicit forall"
adheresToReadShowContrace :: forall a. (Read a, Show a) => a -> Bool
adheresToReadShowContrace val =
  let a = show . read @a . show $ val
      b = show val
   in a == b