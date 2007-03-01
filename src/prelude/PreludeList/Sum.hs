module Prelude where

import Foldl

sum :: (Num a) => [a] -> a
sum = foldl' (+) 0
