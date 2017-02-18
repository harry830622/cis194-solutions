{-# OPTIONS_GHC -Wall #-}

module Calc where

import Data.Maybe
import ExprT
import Parser

-- Exercise 1
eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr str | isNothing expr = Nothing
            | otherwise = Just $ (eval . fromJust) expr
  where expr = parseExp Lit Add Mul str

-- Exercise 3
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit x = Lit x
  add x y = Add x y
  mul x y = Mul x y
