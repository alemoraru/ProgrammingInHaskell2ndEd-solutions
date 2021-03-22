# Exercise 1

Using library functions, define a function halve :: [a] -> ([a],[a]) that
splits an even-lengthed list into two halves.

**Solution**:

```haskell
halve :: [a] -> ([a], [a])
halve xs = splitAt (div (length xs) 2) xs
```

# Exercise 2

Define a function third :: [a] -> a that returns the third element in a list
that contains at least this many elements using:
1. **head** and **tail**;
2. list indexing !!;
3. pattern matching

**Solution**:

```haskell
-- a. Using head and tail
third1 :: [a] -> a
third1 xs = head (tail (tail xs))

-- b. Using list indexing !!
third2 :: [a] -> a
third2 xs = xs !! 2

-- c. Using pattern matching
third3 :: [a] -> a
third3 (_:_:x:_) = x
```

# Exercise 3

Consider a function safetail :: [a] -> [a] that behaves in the same way
as tail except that it maps the empty list to itself rather than producing an
error. Using tail and the function null :: [a] -> Bool that decides if a
list is empty or not, define safetail using:

**Solution**:

```haskell
safetail :: [a] -> [a]
safetail xs | null xs = []
            | otherwise = tail xs
```

# Exercise 4

In a similar way to && in section 4.4, show how the disjunction operator ||
can be defined in four different ways using pattern matching.

**Solution**:

```haskell
(||) :: Bool -> Bool -> Bool
_ || True = True
True || _ = True
_ || _ = False
```

# Exercise 8

The Luhn algorithm is used to check bank card numbers for simple errors
such as mistyping a digit, and proceeds as follows:

* consider each digit as a separate number;
* moving left, double every other number from the second last;
* subtract 9 from each number that is now greater than 9;
* add all the resulting numbers together;
* if the total is divisible by 10, the card number is valid

Define a function luhnDouble :: Int -> Int that doubles a digit and subtracts 9 if the result is greater than 9.

**Solution**:

```haskell
luhnDouble :: Int -> Int
luhnDouble x | (2 * x) > 9 = 2 * x - 9
             | otherwise = 2 * x

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d | (mod ((luhnDouble a) + b + (luhnDouble c) + d) 10) == 0 = True
             | otherwise = False
```