module Prelude where

numericEnumFromThen :: (Real a) => a-> a -> [a]
numericEnumFromThen n m = iterate (+(m-n)) n
