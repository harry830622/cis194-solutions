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

-- Exercise 3
histogram :: [Integer] -> String
histogram integers = (unlines . reverse) $ "0123456789" : "==========" : [] ++
                                           map makeStars
                                               (take maxCount lineCounts)
  where counts = makeCounts integers
        maxCount = fromInteger $ maximum counts
        decrement = map ((+) (-1))
        lineCounts = (iterate decrement counts)

makeCounts :: [Integer] -> [Integer]
makeCounts integers = foldl incrementCounts initialCounts integers
  where initialCounts = replicate 10 0

incrementCounts :: [Integer] -> Integer -> [Integer]
incrementCounts integers x = take n integers ++
                             (integers !! n + 1) : [] ++
                             drop (n + 1) integers
                where n = fromInteger x

makeStars :: [Integer] -> String
makeStars = map (\x -> if x > 0 then '*' else ' ')
