import Prelude hiding (all, any, takeWhile, dropWhile, map, filter, curry, uncurry, iterate)

-- EXERCISE 1

-- List comprehension that maps with f each element of the given list that satisfies p
map_filter_list_comprehension f p xs = [f x | x <- xs, p x]  

-- The same function as above done via high-order functions map and filter
map_filter_high_order f p xs = map f (filter p xs)

-- EXERCISE 2
all :: (a -> Bool) -> [a] -> Bool
all f [] = True
all f (x:xs) | f x = all f xs
             | otherwise = False

any :: (a -> Bool) -> [a] -> Bool
any f [] = False
any f (x:xs) | f x = True
             | otherwise = any f xs

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile f (x:xs) | f x = x : takeWhile f xs
                   | otherwise = []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile f (x:xs) | f x = dropWhile f xs
                   | otherwise = xs

-- EXERCISE 3
map :: (a -> b) -> [a] -> [b]
map f [] = []
map f xs' @ (x:xs) = foldr (\x xs -> (f x) : xs) [] xs'

filter :: (a -> Bool) -> [a] -> [a]
filter p [] = []
filter p xs' @ (x:xs) = foldr (\x xs -> if p x then x : xs else xs) [] xs'

-- EXERCISE 4
dec2int :: [Int] -> Int
dec2int = foldl (\x y -> x * 10 + y) (0)

-- EXERCISE 5

-- Curry redefined
curry :: ((a, b) -> c) -> (a -> b -> c)
curry f = \x y -> f (x, y)

-- Uncurry redefined
uncurry :: (a -> b -> c) -> ((a, b) -> c)
uncurry f = \(x, y) -> f x y

-- EXERCISE 6

unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x | p x       = []
               | otherwise = h x : unfold p h t (t x)

-- Redefine map using unfold
map' :: (a -> b) -> [a] -> [b]
map' f = unfold (null) (f . head) (tail)

-- Redefine iterate using unfold
iterate :: (a -> a) -> a -> [a]
iterate f x = x : unfold (\_ -> False) f f x