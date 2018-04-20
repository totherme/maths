module Maths where

import Control.Monad.State.Lazy

foldPairwise :: (a -> a -> b) -> [a] -> [b]
foldPairwise f = snd . foldl g (Nothing, []) where
  -- g :: (Maybe a, [a]) -> a -> (Maybe a, [a]) -- how to bind this 'a' to the a above?
  g (Nothing, []) x = (Just x, [])
  g (Just y, xs) x = (Just x, xs ++ [f y x])

foldPairwiseRight :: (a -> a -> b) -> [a] -> [b]
foldPairwiseRight f = snd . foldr g (Nothing, []) where
  g x (Nothing, []) = (Just x, [])
  g x (Just y, xs) = (Just x, f x y : xs)

foldPairWiseGen :: Foldable t => (a -> a -> b) -> t a -> [b]
foldPairWiseGen f = snd . foldr g (Nothing, []) where
  g x (Nothing, []) = (Just x, [])
  g x (Just y, xs) = (Just x, f x y : xs)

foldPairwiseRec :: (a -> a -> b) -> [a] -> [b]
foldPairwiseRec f [] = []
foldPairwiseRec f xs = foldPairwiseRec' f (head xs) (tail xs) where
  foldPairwiseRec' f _ [] = []
  foldPairwiseRec' f x (y:ys) = f x y : foldPairwiseRec' f y ys
