# Exercise 1

Show how the list comprehension [f x | x <- xs, p x] can be re-expressed
using the higher-order functions map and filter.

**Solution**:

```haskell
map_filter_high_order f p xs = map f (filter p xs)
```

# Exercise 2

Without looking at the definitions from the standard prelude, define the following higher-order library functions on lists.
1. Decide if all elements of a list satisfy a predicate:
<br>all :: (a -> Bool) -> [Bool] -> Bool
2. Decide if any element of a list satisfies a predicate:
<br>any :: (a -> Bool) -> [Bool] -> Bool
3. Select elements from a list while they satisfy a predicate:
<br>takeWhile :: (a -> Bool) -> [a] -> [a]
4. Remove elements from a list while they satisfy a predicate:
<br>dropWhile :: (a -> Bool) -> [a] -> [a]

**Note**: in the prelude the first two of these functions are generic functions
rather than being specific to the type of lists.

**Solution**:

```haskell
-- a.
all :: (a -> Bool) -> [a] -> Bool
all f [] = True
all f (x:xs) | f x = all f xs
             | otherwise = False

-- b.
any :: (a -> Bool) -> [a] -> Bool
any f [] = False
any f (x:xs) | f x = True
             | otherwise = any f xs

-- c.
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile f (x:xs) | f x = x : takeWhile f xs
                   | otherwise = []

-- d.
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile f (x:xs) | f x = dropWhile f xs
                   | otherwise = xs
```

# Exercise 3

Redefine the functions **map f** and **filter p** using foldr.

**Solution**:

```haskell
map :: (a -> b) -> [a] -> [b]
map f [] = []
map f xs' @ (x:xs) = foldr (\x xs -> (f x) : xs) [] xs'

filter :: (a -> Bool) -> [a] -> [a]
filter p [] = []
filter p xs' @ (x:xs) = foldr (\x xs -> if p x then x : xs else xs) [] xs'
```

# Exercise 4

Using foldl, define a function dec2int :: [Int] -> Int that converts a
decimal number into an integer.

**Solution**:

```haskell
dec2int :: [Int] -> Int
dec2int = foldl (\x y -> x * 10 + y) (0)
```

# Exercise 5

Without looking at the definitions from the standard prelude, define the
higher-order library function curry that converts a function on pairs into
a curried function, and, conversely, the function uncurry that converts a curried function with two arguments into a function on pairs.

**Hint**: first write down the types of the two functions.

**Solution**:

```haskell
-- Curry redefined
curry :: ((a, b) -> c) -> (a -> b -> c)
curry f = \x y -> f (x, y)

-- Uncurry redefined
uncurry :: (a -> b -> c) -> ((a, b) -> c)
uncurry f = \(x, y) -> f x y
```

# Exercise 6

A higher-order function unfold that encapsulates a simple pattern of recursion for producing a list can be defined as follows:
```haskell
unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x | p x       = []
               | otherwise = h x : unfold p h t (t x)
```
That is, the function **unfold p h t** produces the empty list if the predicate **p**
is true of the argument value, and otherwise produces a non-empty list by
applying the function h to this value to give the head, and the function t
to generate another argument that is recursively processed in the same way
to produce the tail of the list. For example, the function **int2bin** can be
rewritten more compactly using unfold as follows:

```haskell
int2bin = unfold (== 0) (`mod` 2) (`div` 2)
```
Redefine the functions **chop8**, **map f** and **iterate f** using **unfold**.


**Solution**:

```haskell
-- Redefine map using unfold
map' :: (a -> b) -> [a] -> [b]
map' f = unfold (null) (f . head) (tail)

-- Redefine iterate using unfold
iterate :: (a -> a) -> a -> [a]
iterate f x = x : unfold (\_ -> False) f f x
```

# Exercise 9

Define a function altMap :: (a -> b) -> (a -> b) -> [a] -> [b] that
alternately applies its two argument functions to successive elements in a list,
in turn about order.

**Solution**:

```haskell
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g xs = [if i `mod` 2 == 1 then f x else g x | (x, i) <- zip xs [1..length xs]]

-- An alternative via pattern matching that doesn't require the use of zip
altMap' :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap' _ _ [] = []
altMap' f g (x:xs) = f x : altMap' g f xs
```

# Exercise 10

Using **altMap**, define a function luhn :: [Int] -> Bool that implements
the *Luhn algorithm* from the exercises in [chapter 4](../Chapter4/chapter4.md) for bank card numbers of
any length. Test your new function using your own bank card.

**Solution**:

```haskell
luhnDouble :: Int -> Int
luhnDouble x = if x < 5 then 2 * x else x * 2 - 9

luhn :: [Int] -> Bool
luhn [] = False
luhn xs = mod (sum (altMap luhnDouble id xs)) 10 == 0
```