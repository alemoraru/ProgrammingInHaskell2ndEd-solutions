-- EXERCISE 1
data Tree a = Leaf | Node (Tree a) a (Tree a)
              deriving Show

instance Functor Tree where
    -- fmap :: (a -> b) -> f a -> f b
    -- fmap :: (a -> b) -> Tree a -> Tree b
    fmap g Leaf         = Leaf
    fmap g (Node l v r) = Node (fmap g l) (g v) (fmap g r)

-- EXERCISE 2
-- instance Functor ((->) a) where
--     -- fmap :: (a -> b) f a -> f b
--     -- fmap :: (b -> c) (a -> b) -> (a -> c)
--     fmap = (.)

-- EXERCISE 4
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

-- EXERCISE 7
data Expr a = Var a | Val Int | Add (Expr a) (Expr a)
              deriving Show

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


