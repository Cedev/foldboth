{-# LANGUAGE TemplateHaskell #-}

import Data.Foldable
import Data.List
import Data.List.Fused (Fused)
import qualified Data.List.Fused as Fused

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

prop_concat :: [[Int]] -> Bool
prop_concat xs = Fused.concat (mkFF xs) ~== concat xs

prop_cons :: Int -> [Int] -> Bool
prop_cons x xs = x `Fused.cons` mkF xs ~== x : xs

prop_snoc :: [Int] -> Int -> Bool
prop_snoc xs x = mkF xs `Fused.snoc` x ~== xs ++ [x]

prop_take :: Int -> [Int] -> Bool
prop_take n xs = Fused.take n (mkF xs) ~== take n xs

prop_take_infinite :: Int -> [Int] -> Bool
prop_take_infinite n xs = Fused.take n (mkF xss) ~== take n xss
  where
    xss = cycle (0:xs)

prop_drop :: Int -> [Int] -> Bool
prop_drop n xs = Fused.drop n (mkF xs) ~== drop n xs

prop_group :: [Bool] -> Bool   
prop_group xs = Fused.group (mkF xs) ~~== group xs
