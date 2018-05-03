
module Data.List.FoldBoth where

-- todo: reimplement in terms of foldr so that it can fuse, be defined for all Foldables
foldboth :: (l -> a -> l) -> (l -> a -> r -> r) -> l -> r -> [a] -> r
foldboth op f l0 r0 = go l0
  where
    go l [] = r0
    go l (x:xs) = let l' = l `op` x in f l' x (go l' xs)
    
foldboth' :: (l -> a -> l) -> (l -> a -> r -> r) -> l -> r -> [a] -> r
foldboth' f = foldboth (\l x -> l `seq` f l x )
