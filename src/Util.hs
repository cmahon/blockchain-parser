module Util where

maxBy :: Ord a => (t -> a) -> t -> t -> t
maxBy f a b
  | f a > f b = a 
  | otherwise = b