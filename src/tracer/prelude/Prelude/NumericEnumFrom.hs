module Prelude where

numericEnumFrom :: (Real a) => a -> [a]
numericEnumFrom n = iterate (+1) n
