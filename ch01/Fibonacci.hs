module Fibonacci where

fibonacciInternal soughtIdx currIdx prevNum currNum =
  if soughtIdx == currIdx
    then prevNum
    else
      let nextIdx = currIdx + 1
          nextNum = prevNum + currNum
       in fibonacciInternal soughtIdx nextIdx currNum nextNum

fibonacci soughtIdx =
  fibonacciInternal soughtIdx 0 0 1
