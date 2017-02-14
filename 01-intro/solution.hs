{-# OPTIONS_GHC -Wall #-}

-- Exercise 1
toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = toDigits (n `div` 10) ++ ((n `mod` 10) : [])

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)

-- Exercise 2
doubleEveryOtherAux :: [Integer] -> [Integer]
doubleEveryOtherAux [] = []
doubleEveryOtherAux (x : []) = x : []
doubleEveryOtherAux (x : y : zs) = x : y * 2 : (doubleEveryOtherAux zs)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther l = reverse (doubleEveryOtherAux (reverse l))

-- Exercise 3
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x : ys) = sum (toDigits x) + (sumDigits ys)

-- Exercise 4
validate :: Integer -> Bool
validate n = sumDigits (doubleEveryOther (toDigits n)) `mod` 10 == 0

-- Exercise 5
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = (hanoi (n - 1) a c b) ++ ((a, b) : []) ++ (hanoi (n - 1) c b a)

-- Exercise 6
-- hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
