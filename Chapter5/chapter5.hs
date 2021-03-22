import Prelude hiding (replicate)

-- EXERCISE 1
sumSquared100 = sum [x^2 | x <- [1..100]]

-- EXERCISE 2
grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x, y) | x <- [0..m], y <- [0..n]]

-- EXERCISE 3
square :: Int -> [(Int, Int)]
square n = [(x, y) | (x, y) <- grid n n, x /= y]

-- EXERCISE 4
replicate :: Int -> a -> [a]
replicate n x = [x | i <- [1..n]]

-- EXERCISE 5
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

-- EXERCISE 6
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], (sum (factors x) - x) == x]

-- EXERCISE 9
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys | length xs == length ys = sum [x * y | (x, y) <- zip xs ys]
                    | otherwise = 0