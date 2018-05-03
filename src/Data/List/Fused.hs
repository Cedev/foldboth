{-# LANGUAGE RankNTypes #-}

module Data.List.Fused (
    Fused(..),
    fuseboth,
    empty,
    
    map,
    concat,
    concatMap,
    
    scanl,
    scanl',
    scanr,
    
    filter,
    takeWhile,
    dropWhile,
    take,
    drop,
    
    cons,
    snoc,
    iterate,
    iterate',
    repeat,
    replicate,
    cycle,
    
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

import Prelude hiding (map, concat, concatMap, scanl, scanl', scanr, filter, takeWhile, dropWhile, take, drop, iterate, iterate', repeat, replicate, cycle,)
import qualified Prelude

import qualified Data.Foldable

import Control.Monad (ap)
import Control.Applicative 

newtype Fused a = Fused {fused :: forall l r. (l -> a -> r -> (l, r)) -> l -> r -> (l, r)}

{-# INLINE fuseboth #-}
fuseboth :: (l -> a -> r -> (l, r)) -> l -> r -> Fused a -> (l, r)
fuseboth f lz rz xs = fused xs f lz rz

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

instance Alternative Fused where
    empty = Fused (\_ l r -> (l, r))
    xs <|> ys = Fused (\f lz rz -> 
        let
          (l1, r1) = fused xs f lz r2
          (l2, r2) = fused ys f l1 rz
          in (l2, r1))

{-# INLINE map #-}
map :: (a -> b) -> Fused a -> Fused b
map f xs = Fused (\g -> fused xs (\l x r -> g l (f x) r))

{-# INLINE concat #-}
concat :: Fused (Fused a) -> Fused a
concat xs = Fused (\f -> fused xs (\l ys r -> fused ys f l r))

{-# INLINE concatMap #-}
concatMap :: (a -> Fused b) -> Fused a -> Fused b
concatMap k xs = Fused (\f -> fused xs (\l x r -> fused (k x) f l r))


{-# INLINE scanl #-}
scanl :: (b -> a -> b) -> b -> Fused a -> Fused b
scanl s z xs = Fused (\f lz rz -> 
                        let (l1, r1) = f lz z r2
                            ((_, l2), r2) = fused xs step (z, l1) rz
                            step (acc, l) x r =
                                let acc' = s acc x
                                    (l1, r1) = f l acc' r
                                    in ((acc', l1), r1)
                            in (l2, r1))
                            
{-# INLINE scanl' #-}
scanl' :: (b -> a -> b) -> b -> Fused a -> Fused b
scanl' f = scanl (\b a -> b `seq` f b a)

{-# INLINE scanr #-}
scanr :: (a -> b -> b) -> b -> Fused a -> Fused b
scanr s z xs = Fused (\f lz rz -> 
                        let (l1, (_, r1)) = fused xs step lz (z, rz)
                            (l2, r2) = f l1 z rz
                            step l x (acc, r) =
                                let acc' = s x acc
                                    (l1, r1) = f l acc' r
                                    in (l1, (acc', r1))
                            in (l2, r1))




{-# INLINE filter #-}
filter :: (a -> Bool) -> Fused a -> Fused a
filter p xs = Fused (\f -> fused xs (\l x r -> if p x then f l x r else (l, r)))


{-# INLINE takeWhile #-}
takeWhile :: (a -> Bool) -> Fused a -> Fused a
takeWhile p xs = Fused (\f lz rz -> 
                          let
                            (l1, (lf, r1)) = fused xs takeStep lz (l1, rz)
                            takeStep l x (lf, r) = 
                                if p x
                                then let (l1, r1) = f l x r in (l1, (lf, r1))
                                else (error "takeWhile", (l, rz))
                            in (lf, r1))


{-# INLINE dropWhile #-}
dropWhile :: (a -> Bool) -> Fused a -> Fused a
dropWhile p xs = Fused (\f lz rz -> 
                          let
                            ((_, lf), rf) = fused xs dropStep (True, lz) rz
                            dropStep (b, l) x r = 
                                if b && p x
                                then ((True, l), r)
                                else let (l1, r1) = f l x r in ((False, l1), r1)
                            in (lf, rf))

{-# INLINE take #-}
take :: Int -> Fused a -> Fused a
take nz xs = Fused (\f lz rz -> 
                          let
                            ((_, l1), (lf, r1)) = fused xs takeStep (nz, lz) (l1, rz)
                            takeStep (n, l) x (lf, r) = 
                                if n > 0
                                then let (l1, r1) = f l x r in ((n - 1, l1), (lf, r1))
                                else (error "take", (l, rz))
                            in (lf, r1))

{-# INLINE drop #-}
drop :: Int -> Fused a -> Fused a
drop nz xs = Fused (\f lz rz -> 
                          let
                            ((_, lf), rf) = fused xs dropStep (nz, lz) rz
                            dropStep (n, l) x r = 
                                if n > 0
                                then ((n - 1, l), r)
                                else let (l1, r1) = f l x r in ((0, l1), r1)
                            in (lf, rf))

{-# INLINE cons #-}
cons :: a -> Fused a -> Fused a
cons x xs = Fused (\f l r ->
    let (l1, r1) = f l x r2
        (l2, r2) = fused xs f l1 r
        in (l2, r1))

{-# INLINE snoc #-}
snoc :: Fused a -> a -> Fused a
snoc xs x = Fused (\f l r ->
    let (l1, r1) = fused xs f l r2
        (l2, r2) = f l1 x r
        in (l2, r1))

{-# INLINE iterate #-}
iterate :: (a -> a) -> a -> Fused a
iterate f = go
  where
    go x = x `cons` go (f x)
    
{-# INLINE iterate' #-}
iterate' :: (a -> a) -> a -> Fused a
iterate' f = iterate (\x -> x `seq` f x)

{-# INLINE repeat #-}
repeat :: a -> Fused a
repeat x = xs where xs = x `cons` xs


{-# INLINE replicate #-}
replicate :: Int -> a -> Fused a
replicate n x = take n (repeat x)


{-# INLINE cycle #-}
cycle :: Fused a -> Fused a
cycle x = xs where xs = x <|> xs
