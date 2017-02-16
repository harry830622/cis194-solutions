{-# OPTIONS_GHC -Wall #-}

module Golf where

-- Exercise 1
skips :: [a] -> [[a]]
skips elements = [every i elements | i <- [1 .. (length elements)]]

every :: Int -> [a] -> [a]
every n elements =
  [elements !! i | i <- [n - 1, n - 1 + n .. (length elements) - 1]]
