module Errors where

data MyEither a b = MyLeft a | MyRight b

foo :: String -> Either String Int
foo name =
  if name == "steve"
    then Left "never forgive you steve"
    else Right $ "hello" <> name