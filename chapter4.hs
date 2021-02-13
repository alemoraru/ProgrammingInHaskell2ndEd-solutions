import Prelude hiding ((||))

-- Exercise 1
halve :: [a] -> ([a], [a])
halve xs = splitAt (div (length xs) 2) xs

-- Exercise 2
-- a. Using head and tail
third1 :: [a] -> a
third1 xs = head (tail (tail xs))

-- b. Using list indexing !!
third2 :: [a] -> a
third2 xs = xs !! 2

-- c. Using pattern matching
third3 :: [a] -> a
third3 (_:_:x:_) = x

-- Exercise 3
safetail :: [a] -> [a]
safetail xs | null xs = []
            | otherwise = tail xs

-- Exercise 4
(||) :: Bool -> Bool -> Bool
_ || True = True
True || _ = True
_ || _ = False

-- Exercise 8
luhnDouble :: Int -> Int
luhnDouble x | (2 * x) > 9 = 2 * x - 9
             | otherwise = 2 * x

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d | (mod ((luhnDouble a) + b + (luhnDouble c) + d) 10) == 0 = True
             | otherwise = False