# Exercise 1

In a similar manner to the function **add**, define a recursive multiplication function mult :: Nat -> Nat -> Nat for the recursive type of natural numbers:

**Hint**: make use of **add** in your definition.

**Solution**:

```haskell
mult :: Nat -> Nat -> Nat
mult _ Zero     = Zero
mult m (Succ n) = add m (mult m n)
```

# Exercise 2

Although not included in appendix B, the standard prelude defines
```haskell
data Ordering = LT | EQ | GT
```
together with a function
```haskell
compare :: Ord a => a -> a -> Ordering
```
that decides if one value in an ordered type is less than (**LT**), equal to (**EQ**),
or greater than (**GT**) another value. Using this function, redefine the function
occurs :: Ord a => a -> Tree a -> Bool for search trees. Why is this
new definition more efficient than the original version?

**Solution**:

```haskell
data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)

occurs :: Ord a => a -> Tree a -> Bool
occurs x Empty    = False
occurs x (Leaf y) = (compare x y) == EQ 
occurs x (Node l y r) | (compare x y) == EQ = True
                      | (compare x y) == LT = occurs x l
                      | otherwise           = occurs x r
```

# Exercise 3

Consider the following type of binary trees:
```haskell
data Tree a = Leaf a | Node (Tree a) (Tree a)
```
Let us say that such a tree is *balanced* if the number of leaves in the left and
right subtree of every node differs by at most one, with leaves themselves being trivially balanced. Define a function balanced :: Tree a -> Bool that
decides if a binary tree is balanced or not.

**Hint**: first define a function that returns the number of leaves in a tree.

**Solution**:

```haskell
size :: Tree a -> Int
size Empty        = 0
size (Leaf x)     = 1
size (Node l _ r) = 1 + size l + size r

is_balanced :: Tree a -> Bool
is_balanced Empty        = True 
is_balanced (Leaf _)     = True
is_balanced (Node l _ r) = abs (size l - size r) <= 1 
                           && is_balanced l && is_balanced r
```

# Exercise 4

Define a function balance :: [a] -> Tree a that converts a non-empty
list into a balanced tree. **Hint**: first define a function that splits a list into two
halves whose length differs by at most one.

**Solution**:

```haskell
balance :: [a] -> Tree a
balance xs = balance_helper xs 0 (length xs - 1)

mid :: Int -> Int -> Int
mid l r = (l + r) `div` 2

balance_helper :: [a] -> Int -> Int -> Tree a
balance_helper [] _ _  = Empty
balance_helper [x] _ _ = Leaf x
balance_helper xs l r | l > r = Empty
                      | otherwise = Node (balance_helper xs l ((mid l r) - 1)) 
                                         (xs !! (mid l r)) 
                                         (balance_helper xs ((mid l r) + 1) r)
```

# Exercise 5

Given the type declaration
```haskell
data Expr = Val Int | Add Expr Expr
```
define a higher-order function
```haskell
folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
```
such that **folde f g** replaces each **Val** constructor in an expression by the
function **f**, and each **Add** constructor by the function **g**.

**Solution**:

```haskell
data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g expr = case expr 
                    of Val x    -> f x
                       Add l r  -> g (folde f g l) (folde f g r) 
```

# Exercise 6

Using **folde**, define a function eval :: Expr -> Int that evaluates an expression to an integer value, and a function size :: Expr -> Int that calculates the number of values in an expression.

**Solution**:

```haskell
eval :: Expr -> Int
eval = folde (\x -> x) (+)

sizeE :: Expr -> Int
sizeE = folde (\_ -> 1) (+)
```