module HW4 where

-- Exercise 1
fun1 :: [Integer] -> Integer
fun1 = product . map (+(-2)) . filter even

fun2 :: Integer -> Integer
fun2 =
  sum .
  filter even .
  takeWhile (>1) .
  iterate (\x -> if even x then x `div` 2 else 3 * x + 1)

-- Exercise 2
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

-- foldTree :: [a] -> Tree a

-- Exercise 3
xor :: [Bool] -> Bool
xor = odd . length . filter (\x -> x)

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f x : xs) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base = foldr (flip f) base

-- Exercise 4
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = 2 : (map (\x -> 2 * x + 1) . filter (\x -> notElem x removed)
                       $ [1 .. n])
  where removed = filter (<=n)
                         [i + j + 2 * i * j | i <- [1 .. n], j <- [i .. n]]
