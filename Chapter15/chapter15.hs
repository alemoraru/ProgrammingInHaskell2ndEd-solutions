-- EXERCISE 4
fibs :: [Integer]
fibs = 0 : 1 : [x + y | (x, y) <- zip fibs $ tail fibs]

-- EXERCISE 6
sqroot :: Double -> Double
sqroot n = snd $ last $ takeWhile (\(a, b) -> abs (a - b) > 0.00001) (zip (getAprox n) (tail $ getAprox n))

getAprox :: Double -> [Double]
getAprox n = iterate (\a -> (a + n / a) / 2) 1.0 