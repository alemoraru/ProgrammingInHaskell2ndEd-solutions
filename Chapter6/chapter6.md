# Exercise 1

How does the recursive version of the factorial function behave if applied to a negative argument, such as (-1)? Modify the definition to prohibit negative
arguments by adding a guard to the recursive case.

**Solution**:

```haskell
factorial :: Int -> Int
factorial n | n < 0 = 0
            | n == 0 = 1
            | otherwise = n * factorial (n - 1)
```

# Exercise 2

Define a recursive function sumdown :: Int -> Int that returns the sum
of the non-negative integers from a given value down to zero. For example,
sumdown 3 should return the result 3+2+1+0 “ 6.


**Solution**:

```haskell
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n - 1)
```

# Exercise 3

Define the exponentiation operator ^ for non-negative integers using the same
pattern of recursion as the multiplication operator *, and show how the expression 2 ^ 3 is evaluated using your definition.

**Solution**:

```haskell
(^) :: Int -> Int -> Int
_ ^ 0 = 1
0 ^ _ = 0
m ^ n = m * (m ^ (n - 1))
```

# Exercise 4

Define a recursive function euclid :: Int -> Int -> Int that implements
Euclid’s algorithm for calculating the greatest common divisor of two nonnegative integers: if the two numbers are equal, this number is the result;
otherwise, the smaller number is subtracted from the larger, and the same
process is then repeated.

**Solution**:

```haskell
euclid :: Int -> Int -> Int
euclid m n | m == n = m
           | m < n = euclid m (n - m)
           | m > n = euclid (m - n) n
```

# Exercise 6

Without looking at the definitions from the standard prelude, define the following library functions on lists using recursion.
1. Decide if all logical values in a list are True:
<br>and :: [Bool] -> Bool
2. Concatenate a list of lists:
<br>concat :: [[a]] -> [a]
3. Produce a list with n identical elements:
<br>replicate :: Int -> a -> [a]
4. Select the nth element of a list:
<br>(!!) :: [a] -> Int -> a
5. Decide if a value is an element of a list:
<br>elem :: Eq a => a -> [a] -> Bool

**Note**: most of these functions are defined in the prelude using other library
functions rather than using explicit recursion, and are generic functions rather
than being specific to the type of lists.

**Solution**:

```haskell
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
```

# Exercise 7

Define a recursive function merge :: Ord a => [a] -> [a] -> [a] that
merges two sorted lists to give a single sorted list.

**Note**: your definition should not use other functions on sorted lists such as
**insert** or **isort**, but should be defined using explicit recursion.

**Solution**:

```haskell
merge :: Ord a => [a] -> [a] -> [a]
merge [] y = y
merge x [] = x
merge x' @ (x:xs) y' @ (y:ys) | (x <= y) = x : merge xs y'
                              | otherwise = y : merge x' ys
```

# Exercise 8

**Solution**:

```haskell
halve :: [a] -> ([a], [a])
halve xs = splitAt (div (length xs) 2) xs

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort as) (msort bs)
    where as = fst (halve xs)
          bs = snd (halve xs)
```

# Exercise 9

Using the five-step process, construct the library functions that:
1. calculate the sum of a list of numbers;
2. **take** a given number of elements from the start of a list;
3. select the **last** element of a non-empty list.

**Solution**:

```haskell
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
```