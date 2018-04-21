{-# LANGUAGE ScopedTypeVariables #-}

import Test.Hspec
import Maths
import Test.QuickCheck (property, forAll, listOf1, arbitrary)

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
  describe "generic version" $
    it "works like the foldl one" $ property $
      \(list :: [Int]) -> foldPairWiseGen (+) list `shouldBe` foldPairwise (+) list
