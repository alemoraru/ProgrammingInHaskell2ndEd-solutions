# Exercise 4

Using a list comprehension, define an expression fibs :: [Integer] that
generates the infinite sequence of Fibonacci numbers
<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;0, 1, 1, 2, 3, 5, 8, 13, 21, 34, ...
<br>
using the following simple procedure:
* the first two numbers are 0 and 1;
* the next is the sum of the previous two;
* return to the second step.

**Hint**: make use of the library functions **zip** and **tail**. Note that numbers in the Fibonacci sequence quickly become large, hence the use of the type **Integer** of arbitrary-precision integers above.

**Solution**:

```haskell
fibs :: [Integer]
fibs = 0 : 1 : [x + y | (x, y) <- zip fibs $ tail fibs]
```

# Exercise 6

*Newton’s method* for computing the square root of a (non-negative) floatingpoint number n can be expressed as follows:
* start with an initial approximation to the result;
* given the current approximation a, the next approximation is defined by
the function **next a = (a + n/a) / 2**;
* repeat the second step until the two most recent approximations are within
some desired distance of one another, at which point the most recent
value is returned as the result.

Define a function sqroot :: Double -> Double that implements this procedure. Hint: first produce an infinite list of approximations using the library function **iterate**. For simplicity, take the number 1.0 as the initial approxi- mation, and 0.00001 as the distance value.


**Solution**:

```haskell
sqroot :: Double -> Double
sqroot n = snd $ last $ takeWhile (\(a, b) -> abs (a - b) > 0.00001) (zip (getAprox n) (tail $ getAprox n))

getAprox :: Double -> [Double]
getAprox n = iterate (\a -> (a + n / a) / 2) 1.0 
```