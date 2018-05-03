{-# LANGUAGE RankNTypes #-}

module Data.List.Fused where

newtype Fused a = Fused {fused :: forall l r x y. (a -> x) -> (l -> x -> l) -> (l -> x -> r -> r) -> (l -> x -> r -> y) -> l -> r -> y}

fuseboth :: (l -> a -> l) -> (l -> a -> r -> r) -> (l -> a -> r -> x) -> l -> r -> Fused a -> x
fuseboth fl fr fy l0 r0 xs = fused xs id fl fr fy l0 r0

instance Functor Fused where
    fmap f xs = Fused (\g fl fr l0 r0 -> fused xs (g . f) fl fr l0 r0)

instance Foldable Fused where
    foldr f z = fuseboth const (const f) (\_ _ r -> r) () z
    foldl f z = fuseboth f (\_ _ r -> r) (\l _ _ -> l) z ()
    