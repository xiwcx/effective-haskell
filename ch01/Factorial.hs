module Factorial where

factorialRecurse :: (Eq t, Num t) => t -> t -> t
factorialRecurse total curNum =
  if curNum == 0
    then total
    else
      -- tail call, this actually uses less memory
      -- ideal for something deeply recursive
      -- constant instead of linear memory
      let nextTotal = total * curNum
          nextNum = curNum - 1
       in factorialRecurse nextTotal nextNum

-- idiomatic for providing default value?
factorial =
  factorialRecurse 1

-- simpler, seemingly inefficient code is sometimes
-- easier to read and generally preferrable in MWB
factorialSimple :: (Eq t, Num t) => t -> t
factorialSimple num =
  if num == 0
    then 1
    else
      num * factorialSimple (num - 1)
