import Prelude hiding ((^), (!!), and, concat, replicate, elem)

-- Exercise 1
factorial :: Int -> Int
factorial n | n < 0 = 0
            | n == 0 = 1
            | otherwise = n * factorial (n - 1)

-- Exercise 2
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n - 1)

-- Exercise 3
(^) :: Int -> Int -> Int
_ ^ 0 = 1
0 ^ _ = 0
m ^ n = m * (m ^ (n - 1))

-- Exercise 4
euclid :: Int -> Int -> Int
euclid m n | m == n = m
           | m < n = euclid m (n - m)
           | m > n = euclid (m - n) n

-- Exercise 6 
-- a. Decide if all logical values in a list are True
and :: [Bool] -> Bool
and [] = True
and (x:xs) | x == True = and xs
           | otherwise = False

-- b. Concatenate a list of lists
concat :: [[a]] -> [a]
concat [] = []
concat (xs:xss) = xs ++ concat xss

-- c. Produce a list with n identical elements
replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x = x : replicate (n - 1) x

-- d. Select the nth element of a list
(!!) :: [a] -> Int -> a
(x:xs) !! 0 = x
(_:xs) !! n = xs !! (n - 1)

-- e. Decide if a value is an element of a list
elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem n (x:xs) | n == x = True
              | otherwise = elem n xs

-- Exercise 7
merge :: Ord a => [a] -> [a] -> [a]
merge [] y = y
merge x [] = x
merge x' @ (x:xs) y' @ (y:ys) | (x <= y) = x : merge xs y'
                              | otherwise = y : merge x' ys

-- Exercise 8
halve :: [a] -> ([a], [a])
halve xs = splitAt (div (length xs) 2) xs

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort as) (msort bs)
    where as = fst (halve xs)
          bs = snd (halve xs)

-- Exercise 9
-- a. Calculate the sum of a list of numbers
sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs

-- b. Take a given number of elements from the start of a list
takeFromList :: Int -> [a] -> [a]
takeFromList 0 _ = []
takeFromList n (x:xs) = x : takeFromList (n - 1) xs

-- c. Select the last element of a non-empty list
lastFromList :: [a] -> a
lastFromList (x:xs) | null xs = x
                    | otherwise = lastFromList xs