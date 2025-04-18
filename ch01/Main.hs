module Main where

makeGreeting salutation person =
  salutation <> " " <> person

-- point free version:
makeGreeting' = (<>) . (<> " ")

main = print "nothing to see here"
