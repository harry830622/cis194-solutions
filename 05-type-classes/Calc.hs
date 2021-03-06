{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Calc where

import ExprT
import Parser
import StackVM
import Data.Maybe
import qualified Data.Map as M

-- Exercise 1
eval :: ExprT -> Integer
eval (ExprT.Lit x) = x
eval (ExprT.Add x y) = eval x + eval y
eval (ExprT.Mul x y) = eval x * eval y

-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr str | isNothing expr = Nothing
            | otherwise = Just $ (eval . fromJust) expr
  where expr = parseExp ExprT.Lit ExprT.Add ExprT.Mul str

-- Exercise 3
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = ExprT.Lit
  add = ExprT.Add
  mul = ExprT.Mul

-- Exercise 4
instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (>0)
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add (MinMax x) (MinMax y) = MinMax $ max x y
  mul (MinMax x) (MinMax y) = MinMax $ min x y

instance Expr Mod7 where
  lit x = Mod7 $ x `mod` 7
  add (Mod7 x) (Mod7 y) = Mod7 $ (x + y) `mod` 7
  mul (Mod7 x) (Mod7 y) = Mod7 $ (x * y) `mod` 7

testExp :: Expr a => String -> Maybe a
testExp = parseExp lit add mul

-- Exercise 5
compile :: String -> Maybe Program
compile = parseExp lit add mul

instance Expr Program where
  lit x = (StackVM.PushI x) : []
  add x y = x ++ y ++ (StackVM.Add : [])
  mul x y = x ++ y ++ (StackVM.Mul : [])

-- Exercise 6
class HasVars a where
  var :: String -> a

data VarExprT = Lit Integer
              | Add VarExprT VarExprT
              | Mul VarExprT VarExprT
              | Var String
  deriving (Eq, Show)

instance Expr VarExprT where
  lit = Calc.Lit
  add = Calc.Add
  mul = Calc.Mul

instance HasVars VarExprT where
  var = Calc.Var

-- Why not just write another "eval" function?
instance Expr (M.Map String Integer -> Maybe Integer) where
  lit x = const $ Just x
  add f g = (\m -> if isNothing (f m) || isNothing(g m) then Nothing
                   else Just $ fromJust (f m) + fromJust (g m))
  mul f g = (\m -> if isNothing (f m) || isNothing(g m) then Nothing
                   else Just $ fromJust (f m) * fromJust (g m))

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var str = (\m -> M.lookup str m)

withVars :: [(String, Integer)]
            -> (M.Map String Integer -> Maybe Integer)
            -> Maybe Integer
withVars vs vExp = vExp $ M.fromList vs
