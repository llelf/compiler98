module Prelude where

maximum :: (Ord a) => [a] -> a
maximum [] = error "PreludeList.maximum: empty list"
maximum xs = foldl1 max xs
