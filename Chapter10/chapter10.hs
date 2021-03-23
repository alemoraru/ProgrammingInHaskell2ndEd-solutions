-- EXERCISE 1
putStr' :: String -> IO ()
putStr' str = sequence_ [putChar ch | ch <- str]

-- EXERCISE 4
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
         
-- EXERCISE 6