
module Data.List.FoldBoth where

-- todo: reimplement in terms of foldr so that it can fuse, be defined for all Foldables
foldboth :: (l -> a -> r -> (l, r)) -> l -> r -> [a] -> (l, r)
foldboth f = go
  where
    go l r [] = (l, r)
    go l r (x:xs) = (l'', r'')
      where
        (l', r'') = f l x r'
        (l'', r') = go l' r xs

foldboth' :: (l -> a -> r -> (l , r)) -> l -> r -> [a] -> (l, r)
foldboth' f = foldboth f'
  where
    f' l x r = l `seq` f l x r
