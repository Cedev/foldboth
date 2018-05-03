
module Data.List.FoldBoth where

-- todo: reimplement in terms of foldr so that it can fuse, be defined for all Foldables
-- | Fold from both the left and right sides at the same time.
foldBoth :: (l -> a -> r -> (l, r)) -> l -> r -> [a] -> (l, r)
foldBoth f = go
  where
    go l r [] = (l, r)
    go l r (x:xs) = (l'', r'')
      where
        (l', r'') = f l x r'
        (l'', r') = go l' r xs

-- | A version of 'foldBoth' that is strict in the left argument
foldBoth' :: (l -> a -> r -> (l , r)) -> l -> r -> [a] -> (l, r)
foldBoth' f = foldBoth f'
  where
    f' l x r = l `seq` f l x r
