{-# LANGUAGE ScopedTypeVariables #-}

import Test.Hspec
import Maths
import Test.QuickCheck (property, forAll, listOf1, arbitrary)

main :: IO ()
main = hspec $ do
  describe "FoldPairwise" $ do
    it "reduces length by 1" $ forAll (listOf1 arbitrary) 
      (\(list :: [Int]) -> length (foldPairwise (+) list) `shouldBe` length list - 1)
    context "when called with 'curry snd'" $ do
      it "equals tail" $ forAll (listOf1 arbitrary) 
        (\(list :: [Int]) -> foldPairwise (curry snd) list `shouldBe` tail list)
