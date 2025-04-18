module Factorial where

factorialInternal curNum total =
  if curNum == 0
    then total
    else
      let nextTotal = total * curNum
          nextNum = curNum - 1
       in factorialInternal nextNum nextTotal

-- idiomatic for providing default value?
factorial curNum =
  factorialInternal curNum 1
