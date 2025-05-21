module Exercises where

-- EXERCISE: Planting Trees

data BinaryTree a = Leaf | Branch (BinaryTree a) a (BinaryTree a)
  deriving (Show)

exampleString :: BinaryTree String
exampleString =
  Branch
    (Branch Leaf "b" Leaf)
    "a"
    (Branch (Branch Leaf "d" Leaf) "c" Leaf)

exampleInt :: BinaryTree Int
exampleInt =
  Branch
    (Branch Leaf 1 Leaf)
    2
    (Branch (Branch Leaf 4 Leaf) 3 Leaf)

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Branch left val right) =
  [val] ++ preorder left ++ preorder right

showStringTree :: BinaryTree String -> String
showStringTree = concat . preorder

addElementToIntTree :: BinaryTree Int -> Int -> BinaryTree Int
addElementToIntTree Leaf val = Branch Leaf val Leaf
addElementToIntTree (Branch left nodeVal right) val
  | val < nodeVal = Branch (addElementToIntTree left val) nodeVal right
  | val > nodeVal = Branch left nodeVal (addElementToIntTree right val)
  | otherwise = Branch left nodeVal right

doesIntExist :: BinaryTree Int -> Int -> Bool
doesIntExist Leaf _ = False
doesIntExist (Branch left val right) needle =
  needle == val || doesIntExist left needle || doesIntExist right needle

data Expr
  = Lit Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr

eval :: Expr -> Either String Int
eval expr =
  case expr of
    Lit num -> Right num
    Add arg1 arg2 -> eval' (+) arg1 arg2
    Sub arg1 arg2 -> eval' (-) arg1 arg2
    Mul arg1 arg2 -> eval' (*) arg1 arg2
    Div arg1 arg2 ->
      case arg2 of
        Lit 0 -> Left "error"
        _ -> eval' div arg1 arg2
  where
    eval' :: (Int -> Int -> Int) -> Expr -> Expr -> Either String Int
    eval' operator arg1 arg2 =
      case eval arg1 of
        Left e -> Left e
        Right i -> case eval arg2 of
          Left e -> Left e
          Right i2 -> Right i2
