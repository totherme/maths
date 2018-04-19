module Maths where

import Control.Monad.State.Lazy


foldPairwise :: (a -> a -> a) -> [a] -> [a]
foldPairwise f = snd . foldl g (Nothing, []) where
  -- g :: (Maybe a, [a]) -> a -> (Maybe a, [a]) -- how to bind this 'a' to the a above?
  g (Nothing, []) x = (Just x, [])
  g (Just y, xs) x = (Just x, xs ++ [f y x])
