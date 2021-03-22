-- EXERCISE 1

data Nat = Zero | Succ Nat
  deriving (Show)

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

-- Actual exercise
mult :: Nat -> Nat -> Nat
mult _ Zero     = Zero
mult m (Succ n) = add m (mult m n)

-- EXERCISE 2

data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)

occurs :: Ord a => a -> Tree a -> Bool
occurs x Empty    = False
occurs x (Leaf y) = (compare x y) == EQ 
occurs x (Node l y r) | (compare x y) == EQ = True
                      | (compare x y) == LT = occurs x l
                      | otherwise           = occurs x r

-- EXERCISE 3
size :: Tree a -> Int
size Empty        = 0
size (Leaf x)     = 1
size (Node l _ r) = 1 + size l + size r

is_balanced :: Tree a -> Bool
is_balanced Empty        = True 
is_balanced (Leaf _)     = True
is_balanced (Node l _ r) = abs (size l - size r) <= 1 
                           && is_balanced l && is_balanced r

-- EXERCISE 4
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

-- EXERCISE 5
data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g expr = case expr 
                    of Val x    -> f x
                       Add l r  -> g (folde f g l) (folde f g r) 

-- EXERCISE 6
eval :: Expr -> Int
eval = folde (\x -> x) (+)

sizeE :: Expr -> Int
sizeE = folde (\_ -> 1) (+)