module InferredTypeDemo where

convertViaInt :: forall {a} b. (Integral a, Num b) => a -> b
convertViaInt input = fromIntegral $ fromIntegral @_ @Int input