{-# LANGUAGE ScopedTypeVariables #-}

module PairWise where

import Data.Unfoldable
import Data.Maybe

mapPairWise :: forall a b t . (Foldable t , Unfoldable t) =>
                (a -> a -> b) -> t a -> t b
mapPairWise f = fromJust . fromList . snd . foldr g (Nothing, []) where
  g :: a -> (Maybe a, [b]) -> (Maybe a, [b])
  g x (Nothing, []) = (Just x, [])
  g x (Just y, xs) = (Just x, f x y : xs)
