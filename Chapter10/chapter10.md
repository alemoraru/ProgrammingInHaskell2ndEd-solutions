# Exercise 1

Redefine putStr :: String -> IO () using a list comprehension and the library function sequence_ :: [IO a] -> IO ().

```haskell
putStr' :: String -> IO ()
putStr' str = sequence_ [putChar ch | ch <- str]
```

# Exercise 4

Define an action adder :: IO () that reads a given number of integers from the keyboard, one per line, and displays their sum.

```haskell
readInt :: IO Int
readInt = do
    x <- getLine
    return (read x :: Int)

readInts :: Int -> IO [Int]
readInts 0 = return []
readInts n = do
    x  <- readInt
    xs <- readInts $ n - 1
    return (x : xs)

adder :: IO ()
adder = do
    putStr "How many numbers? "
    n    <- readInt
    nums <- readInts n
    putStrLn ("The total is " ++ (show $ sum nums))
    return ()
```

# Exercise 6

Using **getCh**, define an action readLine :: IO String that behaves in the same way as **getLine**, except that it also permits the delete key to be used to remove characters. Hint: the delete character is **’\DEL’**, and the control character for moving the cursor back one space is **’\b’**.

```haskell

```