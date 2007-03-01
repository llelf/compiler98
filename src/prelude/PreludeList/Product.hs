module Prelude where

import Foldl

product :: (Num a) => [a] -> a
product = foldl' (*) 1
