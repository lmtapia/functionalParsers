module FoldParentheses where

import Parentheses

-- data Parentheses = Match Parentheses Parentheses
--                  | Empty


type ParenthesesAlgebra a = (a -> a -> a, a)

foldP :: ParenthesesAlgebra a -> Parentheses -> a
foldP (match,empty) = fold where
  fold (Match l r) = match (fold l) (fold r)
  fold Empty       = empty

depthP :: Parentheses -> Int
depthP  = foldP (\x y -> max (x+1) y, 0)