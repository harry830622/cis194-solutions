{-# OPTIONS_GHC -Wall #-}

module Golf where

-- Exercise 1

zipIndex :: [a] -> [(Int, a)]
zipIndex = zip [1..]

makeFilter :: Int -> (Int, a) -> Bool
makeFilter 0 _ = False
makeFilter d (idx, _) = idx `mod` d == 0

skips :: [a] -> [[a]]
skips [] = []
skips es =
  [(snd . unzip) $ filter (makeFilter i) (zipIndex es) | i <- [1..(length es)]]
