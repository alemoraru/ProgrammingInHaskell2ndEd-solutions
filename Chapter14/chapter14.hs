-- EXERCISE 1
-- instance (Monoid a, Monoid b) => Monoid (a, b) where
--     -- mempty :: (a, b)
--     mempty = (mempty, mempty)

--     -- mappend :: (a, b) -> (a, b) -> (a, b)
--     (x1, y1) `mappend` (x2, y2) = (x1 `mappend` x2, y1 `mappend` y2)

-- EXERCISE 2

-- instance Monoid b => Monoid (a -> b) where
--     -- mempty :: a -> b
--     mempty = \_ -> mempty

--     -- mappend :: (a -> b) -> (a -> b) -> (a -> b)
--     f `mappend` g = \x -> f x `mappend` g x

-- EXERCISE 3
-- instance Foldable Maybe where
--     -- foldMap :: Monoid b => (a -> b) -> Maybe a -> b
--     foldMap _ Nothing  = mempty
--     foldMap g (Just x) = g x

--     -- foldr :: (a -> b -> b) -> b -> Maybe a -> b
--     foldr _ v Nothing  = v
--     foldr g v (Just x) = g x v

--     -- foldl :: (a -> b -> a) -> a -> Maybe b -> a
--     foldl _ v Nothing  = v
--     foldl g v (Just x) = g v x

-- instance Traversable Maybe where
--     -- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
--     -- traverse :: Applicative f => (a -> f b) -> Maybe a -> f (Maybe b)
--     traverse _ Nothing  = pure Nothing
--     traverse g (Just x) = fmap Just (g x)


-- EXERCISE 4
data Tree a = Leaf | Node (Tree a) a (Tree a)
              deriving Show

instance Foldable Tree where
    -- foldMap :: Monoid b => (a -> b) -> t a -> b
    -- foldMap :: Monoid b => (a -> b) -> Tree a -> b
    foldMap _ Leaf         = mempty
    foldMap g (Node l v r) = foldMap g l `mappend` g v `mappend` foldMap g r

    -- foldr :: (a -> b -> b) -> b -> t a -> b
    -- foldr :: (a -> b -> b) -> b -> Tree a -> b
    foldr _ a Leaf         = a
    foldr g a (Node l v r) = foldr g (foldr g (g v a) r) l

    -- foldl :: (a -> b -> a) -> a -> t b -> a
    -- foldl :: (a -> b -> a) -> a -> Tree b -> a
    foldl _ a Leaf         = a
    foldl g a (Node l v r) = foldl g (foldl g (g a v) l) r