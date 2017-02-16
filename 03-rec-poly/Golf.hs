{-# OPTIONS_GHC -Wall #-}

module Golf where

-- Exercise 1
skips :: [a] -> [[a]]
skips elements = [every i elements | i <- [1 .. (length elements)]]

every :: Int -> [a] -> [a]
every n elements =
  [elements !! i | i <- [n - 1, n - 1 + n .. (length elements) - 1]]

-- Exercise 2
localMaxima :: [Integer] -> [Integer]
localMaxima integers =
  [integers !! i | i <- [1 .. (length integers) - 1],
    not (isGreaterThanNext !! (i - 1)) && isGreaterThanNext !! i]
  where isGreaterThanNext = makeIsGreaterThanNext integers

makeIsGreaterThanNext :: [Integer] -> [Bool]
makeIsGreaterThanNext integers =
  [integers !! i > integers !! (i + 1) | i <- [0 .. (length integers) - 2]] ++
  (False : [])
