module Fibonacci where

fibonacciInternal currIdx prevNum currNum soughtIdx =
  if soughtIdx == currIdx
    then prevNum
    else
      let nextIdx = currIdx + 1
          nextNum = prevNum + currNum
       in fibonacciInternal nextIdx currNum nextNum soughtIdx

fibonacci :: Integer -> Integer
fibonacci =
  fibonacciInternal 0 0 1
