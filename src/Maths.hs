{-# LANGUAGE ScopedTypeVariables #-}

module Maths where

import Control.Monad.State.Lazy

foldPairwise :: forall a b . (a -> a -> b) -> [a] -> [b]
foldPairwise f = snd . foldl g (Nothing, []) where
  g :: (Maybe a, [b]) -> a -> (Maybe a, [b])
  g (Nothing, []) x = (Just x, [])
  g (Just y, xs) x = (Just x, xs ++ [f y x])

foldPairwiseRight :: forall a b . (a -> a -> b) -> [a] -> [b]
foldPairwiseRight f = snd . foldr g (Nothing, []) where
  g :: a -> (Maybe a, [b]) -> (Maybe a, [b])
  g x (Nothing, []) = (Just x, [])
  g x (Just y, xs) = (Just x, f x y : xs)

foldPairWiseGen :: forall a b t . Foldable t => (a -> a -> b) -> t a -> [b]
foldPairWiseGen f = snd . foldr g (Nothing, []) where
  g :: a -> (Maybe a, [b]) -> (Maybe a, [b])
  g x (Nothing, []) = (Just x, [])
  g x (Just y, xs) = (Just x, f x y : xs)

foldPairwiseRec :: forall a b . (a -> a -> b) -> [a] -> [b]
foldPairwiseRec f [] = []
foldPairwiseRec f xs = foldPairwiseRec' f (head xs) (tail xs) where
  foldPairwiseRec' :: (a -> a -> b) -> a -> [a] -> [b]
  foldPairwiseRec' f _ [] = []
  foldPairwiseRec' f x (y:ys) = f x y : foldPairwiseRec' f y ys
