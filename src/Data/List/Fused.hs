{-# LANGUAGE RankNTypes #-}

module Data.List.Fused where

import Prelude hiding (map, concat, scanl, filter)
import qualified Prelude

newtype Fused a = Fused {fused :: forall l r. (l -> a -> r -> (l, r)) -> l -> r -> (l, r)}

{-# INLINE fuseboth #-}
fuseboth :: (l -> a -> r -> (l, r)) -> l -> r -> Fused a -> (l, r)
fuseboth f lz rz xs = fused xs f lz rz

instance Functor Fused where
    fmap = map

instance Foldable Fused where
    foldr f z = snd . fuseboth (\_ a r -> ((), f a r)) () z
    foldl f z = fst . fuseboth (\l a _ -> (f l a, ())) z ()

{-# INLINE map #-}
map :: (a -> b) -> Fused a -> Fused b
map f xs = Fused (\g -> fused xs (\l x r -> g l (f x) r))

{-# INLINE concat #-}
concat :: Fused (Fused a) -> Fused a
concat xs = Fused (\f -> fused xs (\l ys r -> fused ys f l r))

--{-# INLINE scanl #-}
--scanl :: (a -> b -> b) -> b -> Fused a -> Fused b
--scanl s z = Fused (\f lz rz ->  )

{-# INLINE filter #-}
filter :: (a -> Bool) -> Fused a -> Fused a
filter p xs = Fused (\f -> fused xs (\l x r -> if p x then f l x r else (l, r)))

