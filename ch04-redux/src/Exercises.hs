module Exercises where

-- 1.1
data BinaryTree a = Leaf | Branch (BinaryTree a) a (BinaryTree a)
  deriving (Show)

exampleStringTree :: BinaryTree String
exampleStringTree =
  Branch
    (Branch Leaf "b" Leaf)
    "a"
    (Branch (Branch Leaf "d" Leaf) "c" Leaf)

exampleIntTree :: BinaryTree Int
exampleIntTree =
  Branch
    (Branch Leaf 2 Leaf)
    1
    (Branch (Branch Leaf 4 Leaf) 3 Leaf)

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Branch l v r) = [v] ++ preorder l ++ preorder r

myIntercalate :: [a] -> [[a]] -> [a]
myIntercalate a (x : y : ys) = x <> a <> myIntercalate a (y : ys)
myIntercalate _ rest = concat rest

showStringTree :: BinaryTree String -> String
showStringTree = myIntercalate "," . preorder

addElementToIntTree :: BinaryTree Int -> Int -> BinaryTree Int
addElementToIntTree tree n =
  case tree of
    Leaf -> Branch Leaf n Leaf
    Branch l a r
      | n > a -> Branch l a (addElementToIntTree r n)
      | n < a -> Branch (addElementToIntTree l n) a r
      | otherwise -> Branch l a r

showIntTree :: BinaryTree Int -> BinaryTree String
showIntTree Leaf = Leaf
showIntTree (Branch l a r) = Branch (showIntTree l) (show a) (showIntTree r)

doesIntExist :: BinaryTree Int -> Int -> Bool
doesIntExist Leaf _ = False
doesIntExist (Branch l a r) n
  | n > a = doesIntExist r n
  | n < a = doesIntExist l n
  | otherwise = True