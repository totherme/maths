{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

import Test.Hspec
import Maths
import Test.QuickCheck (property, forAll, listOf1, arbitrary)
import GHC.Generics
import Data.Unfoldable

main :: IO ()
main = hspec $
  describe "FoldPairwise" $ do
    it "reduces length by 1" $ forAll (listOf1 arbitrary) $
      \(list :: [Int]) -> length (mapPairWise (+) list) `shouldBe` length list - 1
    context "when called with 'curry snd'" $
      it "equals tail" $ forAll (listOf1 arbitrary) $
        \(list :: [Int]) -> mapPairWise (curry snd) list `shouldBe` tail list
    context "when called with 'curry fst'" $
      it "equals init" $ forAll (listOf1 arbitrary) $
        \(list :: [Int]) -> mapPairWise (curry fst) list
          `shouldBe` 
          init list
    it "works like the list-only spec" $ property $
      \(list :: [Int]) -> mapPairWise (+) list `shouldBe` mapPairWiseSpec (+) list
    it "works on trees as well as lists" $
      mapPairWise (+) (Node (Node (Leaf 1) (Leaf 1)) (Leaf 1))
      `shouldBe`
      Node (Leaf 2) (Leaf 2)

mapPairWiseSpec :: forall a b . (a -> a -> b) -> [a] -> [b]
mapPairWiseSpec f [] = []
mapPairWiseSpec f xs = mapPairWiseSpec' f (head xs) (tail xs) where
  mapPairWiseSpec' :: (a -> a -> b) -> a -> [a] -> [b]
  mapPairWiseSpec' f _ [] = []
  mapPairWiseSpec' f x (y:ys) = f x y : mapPairWiseSpec' f y ys


data BT a = Node (BT a) (BT a) | Leaf a deriving (Generic1, Show, Eq)

instance Unfoldable BT
instance Foldable BT where
  foldr f x (Leaf y) = f y x
  foldr f x (Node t1 t2) = foldr f (foldr f x t1) t2
