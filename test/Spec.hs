{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

import Test.Hspec
import Maths
import Test.QuickCheck (property, forAll, listOf1, arbitrary)
import GHC.Generics
import Data.Unfoldable

main :: IO ()
main = hspec $ do
  describe "FoldPairwise" $ do
    it "reduces length by 1" $ forAll (listOf1 arbitrary) $
      \(list :: [Int]) -> length (foldPairwise (+) list) `shouldBe` length list - 1
    context "when called with 'curry snd'" $
      it "equals tail" $ forAll (listOf1 arbitrary) $
        \(list :: [Int]) -> foldPairwise (curry snd) list `shouldBe` tail list
    context "when called with 'curry fst'" $
      it "equals init" $ forAll (listOf1 arbitrary) $
        \(list :: [Int]) -> foldPairwise (curry fst) list
          `shouldBe` 
          init list
  describe "foldr variation" $
    it "works like the foldl one" $ property $
      \(list :: [Int]) -> foldPairwiseRight (+) list `shouldBe` foldPairwise (+) list
  describe "explicitly recursive variation" $
    it "works like the foldl one" $ property $
      \(list :: [Int]) -> foldPairwiseRec (+) list `shouldBe` foldPairwise (+) list
  describe "generic version" $ do
    it "works like the foldl one" $ property $
      \(list :: [Int]) -> foldPairWiseGeneral (+) list `shouldBe` foldPairwise (+) list
    it "works on trees as well as lists" $
      foldPairWiseGeneral (+) (Node (Node (Leaf 1) (Leaf 1)) (Leaf 1))
      `shouldBe`
      Node (Leaf 2) (Leaf 2)

data BT a = Node (BT a) (BT a) | Leaf a deriving (Generic1, Show, Eq)

instance Unfoldable BT
instance Foldable BT where
  foldr f x (Leaf y) = f y x
  foldr f x (Node t1 t2) = foldr f (foldr f x t1) t2
