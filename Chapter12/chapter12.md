# Exercise 1

Define an instance of the Functor class for the following type of binary trees that have data in their nodes:

```haskell
data Tree a = Leaf | Node (Tree a) a (Tree a)
              deriving Show
```

**Solution**:

```haskell
instance Functor Tree where
    -- fmap :: (a -> b) -> f a -> f b
    -- fmap :: (a -> b) -> Tree a -> Tree b
    fmap g Leaf         = Leaf
    fmap g (Node l v r) = Node (fmap g l) (g v) (fmap g r)
```

# Exercise 2

Complete the following instance declaration to make the partially-applied function type (a ->) into a functor:
```haskell
instance Functor ((->) a) where
    ...
```

**Hint**: first write down the type of **fmap**, and then think if you already know a library function that has this type.

**Solution**:

```haskell
instance Functor ((->) a) where
    -- fmap :: (a -> b) -> f a -> f b
    -- fmap :: (b -> c) -> (a -> b) -> (a -> c)
    fmap = (.)
```

# Exercise 4

There may be more than one way to make a parameterised type into an applicative functor. For example, the library **Control.Applicative** provides an alternative ‘zippy’ instance for lists, in which the function pure makes an infinite list of copies of its argument, and the operator **<*>** applies each
argument function to the corresponding argument value at the same position. Complete the following declarations that implement this idea:

```haskell
newtype ZipList a = Z [a] deriving Show

instance Functor ZipList where
    -- fmap :: (a -> b) -> ZipList a -> ZipList b
    fmap g (Z xs) = ...

instance Applicative ZipList where
    -- pure :: a -> ZipList a
    pure x = ...

    -- <*> :: ZipList (a -> b) -> ZipList a -> ZipList b
    (Z gs) <*> (Z xs) = ...
```

The ZipList wrapper around the list type is required because each type can only have at most one instance declaration for a given class.

**Solution**:

```haskell
newtype ZipList a = Z [a] 
  deriving Show

instance Functor ZipList where
    -- fmap :: (a -> b) -> ZipList a -> ZipList b
    fmap g (Z [])     = Z []
    fmap g (Z (x:xs)) = Z (g x : fmap g xs)

instance Applicative ZipList where
    -- pure :: a -> ZipList a
    pure x = Z (repeat x)

    -- <*> :: ZipList (a -> b) -> ZipList a -> ZipList b
    (Z gs) <*> (Z xs) = Z [g x | (g, x) <- zip gs xs]
```

# Exercise 7

Given the following type of expressions
```haskell
data Expr a = Var a | Val Int | Add (Expr a) (Expr a)
              deriving Show
```
that contain variables of some type a, show how to make this type into instances of the **Functor**, **Applicative** and **Monad** classes. With the aid of an
example, explain what the >>= operator for this type does.

**Solution**

```haskell
instance Functor Expr where
    -- fmap :: (a -> b) -> f a -> f b
    -- fmap :: (a -> b) -> Expr a -> Expr b
    fmap g (Var x)   = Var (g x)
    fmap g (Val x)   = Val x
    fmap g (Add l r) = Add (fmap g l) (fmap g r)

instance Applicative Expr where
    -- pure :: a -> Expr a
    pure x = Var x

    -- <*> :: Expr (a -> b) -> Expr a -> Expr b
    Var g <*> expr = fmap g expr

instance Monad Expr where
    -- (>>=) :: m a -> (a -> m b) -> m b
    -- (>>=) :: Expr a -> (a -> Expr b) -> Expr b
    Var x     >>= g = g x
    Val x     >>= _ = Val x
    Add x y >>= g = Add (x >>= g) (y >>= g)
```

