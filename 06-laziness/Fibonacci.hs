{-# OPTIONS_GHC -Wall #-}

module Fibonacci where

-- Exercise 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 2) + fib (n - 1)

fibs1 :: [Integer]
fibs1 = map fib [0 ..]

-- Exercise 2
fibs2 :: [Integer]
fibs2 = 0 : 1 : [fibs2 !! (i - 2) + fibs2 !! (i - 1) | i <- [2 ..]]
