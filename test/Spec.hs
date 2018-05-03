{-# LANGUAGE TemplateHaskell #-}

import Data.Foldable
import Data.List
import Data.List.Fused (Fused)
import qualified Data.List.Fused as Fused

import Control.Arrow ((&&&))

import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck.Arbitrary

main :: IO ()
main = $(defaultMainGenerator)

instance Arbitrary a => Arbitrary (Fused a) where
    arbitrary = Fused.fromList <$> arbitrary
    shrink = fmap Fused.fromList . shrink . toList
    
mkF = Fused.fromList
mkFF = fmap Fused.fromList . Fused.fromList

xs ~== ys = toList xs == ys
xs ~~== ys = toList (toList <$> xs) == ys

infix 4 ~==
    
prop_fromList_toList :: [Int] -> Bool
prop_fromList_toList xs = toList (Fused.fromList xs) == xs
    
prop_append :: [Int] -> [Int] -> Bool
prop_append xs ys = mkF xs `Fused.append` mkF ys ~== xs ++ ys

prop_append_infinite :: [Int] -> [Int] -> Bool
prop_append_infinite xs ys = Fused.take n (mkF xs `Fused.append` mkF yss) ~== take n (xs ++ yss)
  where
    yss = cycle (0:xs)
    n = 10 + length xs

prop_concat :: [[Int]] -> Bool
prop_concat xs = Fused.concat (mkFF xs) ~== concat xs

prop_concatMap :: Int -> [Int] -> Bool
prop_concatMap x xs = Fused.concatMap (Fused.replicate fanout) (mkF xs) ~== concatMap (replicate fanout) xs
  where
    fanout = x `mod` 5
    
prop_scanl :: Int -> [Int] -> Bool
prop_scanl z xs = Fused.scanl (+) z (mkF xs) ~== scanl (+) z xs

prop_scanl' :: Int -> [Int] -> Bool
prop_scanl' z xs = Fused.scanl' (+) z (mkF xs) ~== scanl' (+) z xs

prop_scanr :: Int -> [Int] -> Bool
prop_scanr z xs = Fused.scanr (+) z (mkF xs) ~== scanr (+) z xs

prop_cons :: Int -> [Int] -> Bool
prop_cons x xs = x `Fused.cons` mkF xs ~== x : xs

prop_snoc :: [Int] -> Int -> Bool
prop_snoc xs x = mkF xs `Fused.snoc` x ~== xs ++ [x]

prop_iterate :: Int -> Int -> Bool
prop_iterate x y = Fused.take 10 (Fused.iterate (x+) y) ~== take 10 (iterate (x+) y)

prop_iterate' :: Int -> Int -> Bool
prop_iterate' x y = Fused.take 10 (Fused.iterate' (x+) y) ~== take 10 (iterate (x+) y)

prop_repeat :: Int -> Bool
prop_repeat x = Fused.take 10 (Fused.repeat x) ~== take 10 (repeat x)

prop_replicate :: Int -> Int -> Bool
prop_replicate n x = Fused.replicate n x ~== replicate n x

prop_cycle :: Int -> [Int] -> Bool
prop_cycle n xs = Fused.take n (Fused.cycle (0 `Fused.cons` mkF xs)) ~== take n (cycle (0 : xs))

prop_take :: Int -> [Int] -> Bool
prop_take n xs = Fused.take n (mkF xs) ~== take n xs

prop_take_infinite :: Int -> [Int] -> Bool
prop_take_infinite n xs = Fused.take n (mkF xss) ~== take n xss
  where
    xss = cycle (0:xs)

prop_drop :: Int -> [Int] -> Bool
prop_drop n xs = Fused.drop n (mkF xs) ~== drop n xs

prop_dropWhile :: Int -> [Int] -> Bool
prop_dropWhile threshold xs = Fused.dropWhile (< threshold) (mkF xs) ~== dropWhile (< threshold) xs

prop_takeWhile :: Int -> [Int] -> Bool
prop_takeWhile threshold xs = Fused.takeWhile (< threshold) (mkF xs) ~== takeWhile (< threshold) xs

prop_takeWhile_infinite :: Int -> [Int] -> Bool
prop_takeWhile_infinite threshold xs = Fused.takeWhile (< threshold) (mkF xss) ~== takeWhile (< threshold) xss
  where
    xss = cycle (xs ++ [threshold + 1])

prop_group :: [Bool] -> Bool   
prop_group xs = Fused.group (mkF xs) ~~== group xs

prop_runLengthEncode :: [Bool] -> Bool
prop_runLengthEncode xs = Fused.runLengthEncode (mkF xs) ~== ((length &&& head) <$> group xs)

prop_intersperse :: Int -> [Int] -> Bool
prop_intersperse x xs = Fused.intersperse x (mkF xs) ~== intersperse x xs

prop_intercalate :: [Int] -> [[Int]] -> Bool
prop_intercalate x xs = Fused.intercalate (mkF x) (mkFF xs) ~== intercalate x xs
