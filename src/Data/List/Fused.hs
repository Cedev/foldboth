{-# LANGUAGE RankNTypes #-}

module Data.List.Fused (
    Fused(..),
    fuseboth,
    empty,
    
    map,
    concat,
    concatMap,
    filter,
    
    Data.Foldable.Foldable(..),
    Data.Foldable.foldrM,
    Data.Foldable.foldlM,
    Data.Foldable.traverse_,
    Data.Foldable.for_,
    Data.Foldable.sequenceA_,
    Data.Foldable.asum,
    Data.Foldable.mapM_,
    Data.Foldable.forM_,
    Data.Foldable.sequence_,
    Data.Foldable.msum,
    Data.Foldable.and,
    Data.Foldable.or,
    Data.Foldable.any,
    Data.Foldable.all,
    Data.Foldable.maximumBy,
    Data.Foldable.minimumBy,
    Data.Foldable.notElem,
    Data.Foldable.find
) where

import Prelude hiding (map, concat, concatMap, scanl, filter)
import qualified Prelude

import qualified Data.Foldable

import Control.Monad (ap)

newtype Fused a = Fused {fused :: forall l r. (l -> a -> r -> (l, r)) -> l -> r -> (l, r)}

{-# INLINE fuseboth #-}
fuseboth :: (l -> a -> r -> (l, r)) -> l -> r -> Fused a -> (l, r)
fuseboth f lz rz xs = fused xs f lz rz

empty :: Fused a
empty = Fused (\_ l r -> (l, r))

instance Functor Fused where
    fmap = map

instance Foldable Fused where
    foldr f z = snd . fuseboth (\_ a r -> ((), f a r)) () z
    foldl f z = fst . fuseboth (\l a _ -> (f l a, ())) z ()

-- There's a Traversable instance, but it doesn't fuse

instance Applicative Fused where
    pure x = Fused (\f l r -> f l x r)
    (<*>) = ap
    
instance Monad Fused where
    return = pure
    (>>=) = flip concatMap

    
{-# INLINE map #-}
map :: (a -> b) -> Fused a -> Fused b
map f xs = Fused (\g -> fused xs (\l x r -> g l (f x) r))

{-# INLINE concat #-}
concat :: Fused (Fused a) -> Fused a
concat xs = Fused (\f -> fused xs (\l ys r -> fused ys f l r))

{-# INLINE concatMap #-}
concatMap :: (a -> Fused b) -> Fused a -> Fused b
concatMap k xs = Fused (\f -> fused xs (\l x r -> fused (k x) f l r))

--{-# INLINE scanl #-}
--scanl :: (a -> b -> b) -> b -> Fused a -> Fused b
--scanl s z = Fused (\f lz rz ->  )

{-# INLINE filter #-}
filter :: (a -> Bool) -> Fused a -> Fused a
filter p xs = Fused (\f -> fused xs (\l x r -> if p x then f l x r else (l, r)))

