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
      \(list :: [Int]) -> length (foldPairWise (+) list) `shouldBe` length list - 1
    context "when called with 'curry snd'" $
      it "equals tail" $ forAll (listOf1 arbitrary) $
        \(list :: [Int]) -> foldPairWise (curry snd) list `shouldBe` tail list
    context "when called with 'curry fst'" $
      it "equals init" $ forAll (listOf1 arbitrary) $
        \(list :: [Int]) -> foldPairWise (curry fst) list
          `shouldBe` 
          init list
    it "works like the list-only spec" $ property $
      \(list :: [Int]) -> foldPairWise (+) list `shouldBe` foldPairwiseSpec (+) list
    it "works on trees as well as lists" $
      foldPairWise (+) (Node (Node (Leaf 1) (Leaf 1)) (Leaf 1))
      `shouldBe`
      Node (Leaf 2) (Leaf 2)

foldPairwiseSpec :: forall a b . (a -> a -> b) -> [a] -> [b]
foldPairwiseSpec f [] = []
foldPairwiseSpec f xs = foldPairwiseSpec' f (head xs) (tail xs) where
  foldPairwiseSpec' :: (a -> a -> b) -> a -> [a] -> [b]
  foldPairwiseSpec' f _ [] = []
  foldPairwiseSpec' f x (y:ys) = f x y : foldPairwiseSpec' f y ys


data BT a = Node (BT a) (BT a) | Leaf a deriving (Generic1, Show, Eq)

instance Unfoldable BT
instance Foldable BT where
  foldr f x (Leaf y) = f y x
  foldr f x (Node t1 t2) = foldr f (foldr f x t1) t2
