
module Data.List.FoldBoth (
  -- * Bi-directional folds
  foldBoth,
  foldBoth',
  foldBothBy,
  foldBothBy',
  -- * Convenience functions
  folded,
  foldedBy,
) where

import GHC.Exts (oneShot)

-- | Fold from both the left and right sides at the same time.
{-# INLINE foldBoth #-}
foldBoth :: Foldable t => (l -> a -> (l, r -> r)) -> l -> r -> t a -> (l, r)
foldBoth k lz rz xs = foldBothBy (folded xs) k lz rz

-- | A version of 'foldBoth' that is strict in the left argument
{-# INLINE foldBoth' #-}
foldBoth' :: Foldable t =>  (l -> a -> (l, r -> r)) -> l -> r -> t a -> (l, r)
foldBoth' k lz rz xs = foldBothBy' (folded xs) k lz rz
    
type FoldR a b = (a -> b -> b) -> b -> b

-- | A version of 'foldBoth' that doesn't require a Foldable
{-# INLINE foldBothBy #-}
foldBothBy :: FoldR a (l -> (l, r)) ->  (l -> a -> (l, r -> r)) -> l -> r -> (l, r)
foldBothBy foldr k lz rz = 
  foldr (\x fn -> oneShot (\l -> 
                                let (l1, r) = fn l2
                                    (l2, fr) = k l x
                                    in (l1, fr r))) (\l -> (l, rz)) lz
                                    
-- | Strict version of 'foldBothBy''
{-# INLINE foldBothBy' #-}
foldBothBy' :: FoldR a (l -> (l, r)) -> (l -> a -> (l, r -> r)) -> l -> r -> (l, r)
foldBothBy' foldr k lz rz = 
  foldr (\x fn -> oneShot (\l -> l `seq`
                                let (l1, r) = fn l2
                                    (l2, fr) = k l x
                                    in (l1, fr r))) (\l -> (l, rz)) lz
                                    
-- | Rearrange the arguments to 'foldr' so that the structure it acts on is the first argument
{-# INLINE folded #-}
folded :: Foldable t => t a -> FoldR a b
folded = foldedBy foldr

-- | Rearrange the arguments to a foldr-like function so that the structure it acts on is the first argument
{-# INLINE foldedBy #-}
foldedBy :: ( (a -> b -> b) -> b -> xs -> b ) -> xs -> FoldR a b
foldedBy foldr xs k z = foldr k z xs
