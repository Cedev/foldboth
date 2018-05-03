{-# LANGUAGE RankNTypes #-}

module Data.List.Fused (
    Fused(..),
    fuseBoth,
    fromList,
    
    map,
    concat,
    concatMap,
    append,
    
    -- * Scans
    scanl,
    scanl',
    scanr,
    mapAccumL,
    mapAccumR,
    
    -- * Filtering
    filter,
    takeWhile,
    dropWhile,
    take,
    drop,
    
    -- * Extrema
    extremaBy,
    minimums,
    maximums,
    
    -- * Grouping
    runLengthEncode,
    group,
    
    -- * Transformations
    intersperse,
    intercalate,
    
    -- * Construction
    cons,
    snoc,
    iterate,
    iterate',
    repeat,
    replicate,
    cycle,
    
    -- * Re-exports
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
    Data.Foldable.find,
) where

import Prelude hiding (
    map,
    concat,
    concatMap,
    
    scanl,
    scanl',
    scanr,
    mapAccumL,
    mapAccumR,
    
    filter,
    takeWhile,
    dropWhile,
    take,
    drop,
    
    iterate,
    iterate',
    repeat,
    replicate,
    cycle)

import qualified Prelude

import qualified Data.Foldable

import Control.Arrow (first)
import Control.Applicative 
import Control.Monad

import Data.List.FoldBoth

-- | A list represented by its 'foldBoth' function.
newtype Fused a = Fused {fused :: forall l r. (l -> a -> r -> (l, r)) -> l -> r -> (l, r)}

{-# INLINE fromList #-}
-- | Convert a list to 'Fused' via its 'foldBoth' function
fromList :: [a] -> Fused a
fromList xs = Fused (\f l r -> foldBoth f l r xs)

{-# INLINE fuseBoth #-}
fuseBoth :: (l -> a -> r -> (l, r)) -> l -> r -> Fused a -> (l, r)
fuseBoth f lz rz xs = fused xs f lz rz

instance Functor Fused where
    fmap = map

instance Foldable Fused where
    foldr f z = snd . fuseBoth (\_ a r -> ((), f a r)) () z
    foldl f z = fst . fuseBoth (\l a _ -> (f l a, ())) z ()

-- The traversable instance doesn't fuse
instance Traversable Fused where
    traverse k = fmap fromList . traverse k . Data.Foldable.toList

instance Applicative Fused where
    pure x = Fused (\f l r -> f l x r)
    (<*>) = ap
    
instance Monad Fused where
    return = pure
    (>>=) = flip concatMap

instance Alternative Fused where
    empty = Fused (\_ l r -> (l, r))
    (<|>) = append
          
instance MonadPlus Fused

instance Show a => Show (Fused a) where
    show xs = "fromList " ++ show (Data.Foldable.toList xs)

instance Read a => Read (Fused a) where
    readsPrec p ('f':'r':'o':'m':'L':'i':'s':'t':' ':xs) = first fromList <$> readsPrec p xs
    readsPrec _ _ = []


{-# INLINE map #-}
map :: (a -> b) -> Fused a -> Fused b
map f xs = Fused (\g -> fused xs (\l x r -> g l (f x) r))

{-# INLINE concat #-}
concat :: Fused (Fused a) -> Fused a
concat xs = Fused (\f -> fused xs (\l ys r -> fused ys f l r))

{-# INLINE concatMap #-}
concatMap :: (a -> Fused b) -> Fused a -> Fused b
concatMap k xs = Fused (\f -> fused xs (\l x r -> fused (k x) f l r))

{-# INLINE append #-}
append :: Fused a -> Fused a -> Fused a
append xs ys = Fused (\f lz rz -> 
                    let
                        (l1, r1) = fused xs f lz r2
                        (l2, r2) = fused ys f l1 rz
                        in (l2, r1))


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

-- TODO: easy ways to peek at adjacent items (scanl1, scanr1)

{-# INLINE mapAccumL #-}
mapAccumL :: (a -> b -> (a, c)) -> a -> Fused b -> Fused c
mapAccumL s z xs = Fused (\f lz rz ->
                            let ((_, lf), rf) = fused xs step (z, lz) rz
                                step (acc, l) x r =
                                    let (acc', y) = s acc x
                                        (l', r') = f l y r
                                        in ((acc', l'), r')
                                in (lf, rf))

{-# INLINE mapAccumR #-}
mapAccumR :: (a -> b -> (a, c)) -> a -> Fused b -> Fused c
mapAccumR s z xs = Fused (\f lz rz ->
                            let (lf, (_, rf)) = fused xs step lz (z, rz)
                                step l x (acc, r) =
                                    let (acc', y) = s acc x
                                        (l', r') = f l y r
                                        in (l', (acc', r'))
                                in (lf, rf))

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
-- | The strict version of 'iterate'
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


{-# INLINE intersperse #-}
intersperse :: a -> Fused a -> Fused a
intersperse y xs = Fused (\f lz rz ->
                            let ((_, lf), rf) = fused xs step (False, lz) rz
                                step (prepend, l) x r = 
                                    let (l', r') =
                                            if prepend 
                                            then
                                                let
                                                    (l1, r1) = f l y r2
                                                    (l2, r2) = f l1 x r
                                                    in (l2, r1)
                                            else f l x r
                                        in ((True, l'), r')
                                in (lf, rf))

{-# INLINE intercalate #-}
intercalate :: Fused a -> Fused (Fused a) -> Fused a
intercalate x xs = concat (intersperse x xs)



-- minimums

{-# INLINE extremaBy #-}
-- | Compute extrema of a fused list. @'extremaBy' cmp@ will including a value @x@ if @x \`cmp\` y@ holds for the previous extreme value @y@.
extremaBy :: (a -> a -> Bool) -> Fused a -> Fused a
extremaBy cmp xs = Fused (\f lz rz -> 
                    let ((_, lf), rf) = fused xs step ((const True), lz) rz
                        step (include, l) x r =
                            if include x
                            then let (l1, r1) = f l x r in (((`cmp` x), l1), r1)
                            else ((include, l), r)
                        in (lf, rf))

{-# INLINE minimums #-}
-- | Minimums of a list, equivalent to @'extremaBy' ('<')@.
minimums :: Ord a => Fused a -> Fused a
minimums = extremaBy (<)

{-# INLINE maximums #-}
-- | Maximums of a list, equivalent to @'extremaBy' ('>')@.
maximums :: Ord a => Fused a -> Fused a
maximums = extremaBy (>)


-- run length encode
{-# INLINE runLengthEncode #-}
runLengthEncode :: Eq a => Fused a -> Fused (Int, a)
runLengthEncode xs = Fused (\f lz rz -> 
                    let ((_, cf, lf), (_, rf)) = fused xs step ((const True), 0, lz) (cf, rz)
                        step (split, runlength, l) x (futurelength, r) =
                            if split x
                            then let (l1, r1) = f l (futurelength, x) r in (((/= x), 1, l1), (runlength, r1))
                            else ((split, runlength, l), (futurelength, r))
                    in (lf, rf))

{-# INLINE group #-}
group :: Eq a => Fused a -> Fused (Fused a)
group xs = Fused (\f lz rz -> 
                    let ((_, lf), (_, rf)) = fused xs step ((const True), lz) (empty, rz)
                        step (split, l) x (xs, r) =
                            if split x
                            then let (l1, r1) = f l (x `cons` xs) r in (((/= x), l1), (empty, r1))
                            else ((split, l), (x `cons` xs, r))
                    in (lf, rf))
