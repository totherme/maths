module Maths where

import Control.Monad.State.Lazy


foldPairwise :: (a -> a -> a) -> [a] -> [a]
foldPairwise f = snd . foldl g (Nothing, []) where
  -- g :: (Maybe a, [a]) -> a -> (Maybe a, [a]) -- how to bind this 'a' to the a above?
  g (Nothing, []) x = (Just x, [])
  g (Just y, xs) x = (Just x, xs ++ [f y x])

foldPairwiseRight :: (a -> a -> a) -> [a] -> [a]
foldPairwiseRight f = snd . foldr g (Nothing, []) where
  g x (Nothing, []) = (Just x, [])
  g x (Just y, xs) = (Just x, f x y : xs)

foldPairwiseRec :: (a -> a -> a) -> [a] -> [a]
foldPairwiseRec f [] = []
foldPairwiseRec f xs = foldPairwiseRec' f (head xs) (tail xs) where
  foldPairwiseRec' f _ [] = []
  foldPairwiseRec' f x (y:ys) = f x y : foldPairwiseRec' f y ys
